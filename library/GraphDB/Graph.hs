-- |
-- A graph API over monomorphic values.
module GraphDB.Graph where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as V
import qualified HashtablesPlus as H
import qualified HashtablesPlus.HashRef as HR


type Basic = H.Basic
type Cuckoo = H.Cuckoo
type Linear = H.Linear

class (H.Algorithm (Algorithm s), H.Key (Index s), Serializable IO (Value s)) => Setup s where
  -- |
  -- Any of the "hashtables-plus" algorithms:
  -- 'H.Basic', 'H.Cuckoo', 'H.Linear'.
  -- 
  -- Affects performance and memory footprint of the graph.
  type Algorithm s :: * -> * -> * -> *
  data Index s
  data Value s
  indexes :: Value s -> Value s -> [Index s]

data Refs s =
  Refs {
    refsValue :: {-# UNPACK #-} !(IORef (Value s)),
    refsTargets :: !(H.Multimap (Algorithm s) (Index s) (H.HashRefSet (Algorithm s) (Refs s))),
    refsSources :: !(H.HashRefSet (Algorithm s) (Refs s))
  }

type Node s = HR.HashRef (Refs s)


-- * Operations
-------------------------

new :: (Setup s) => Value s -> IO (Node s)
new v = HR.new =<< Refs <$> newIORef v <*> H.new <*> H.new

getValue :: Node s -> IO (Value s)
getValue = readIORef . refsValue . HR.value

setValue :: (Setup s) => Node s -> Value s -> IO ()
setValue node newValue = do
  oldValue <- getValue node
  H.traverse (refsSources $ HR.value $ node) $ \source -> do
    sourceValue <- getValue source
    forM_ (indexes oldValue sourceValue) $ \i -> do
      H.deleteFast (refsTargets $ HR.value $ source) (i, node)
    forM_ (indexes newValue sourceValue) $ \i -> do
      H.insertFast (refsTargets $ HR.value $ source) (i, node)
  writeIORef (refsValue $ HR.value $ node) newValue

addTarget :: (Setup s) => Node s -> Node s -> IO ()
addTarget source target = do
  il <- pure indexes <*> getValue target <*> getValue source
  forM_ il $ \i -> H.insertFast (refsTargets . HR.value $ source) (i, target)
  H.insertFast (refsSources $ HR.value $ target) source

removeTarget :: (Setup s) => Node s -> Node s -> IO ()
removeTarget source target = do
  il <- pure indexes <*> getValue target <*> getValue source
  forM_ il $ \i -> H.deleteFast (refsTargets $ HR.value $ source) (i, target) 
  H.deleteFast (refsSources $ HR.value $ target) source

traverseTargetsByIndex :: (Setup s) => Node s -> Index s -> (Node s -> IO ()) -> IO ()
traverseTargetsByIndex source index = H.traverseMulti (refsTargets $ HR.value $ source) index

traverseTargets :: (Setup s) => Node s -> (Node s -> IO ()) -> IO ()
traverseTargets source f = do
  visited :: H.Set H.Basic (Node s) <- H.new
  H.traverse (refsTargets $ HR.value $ source) $ \(i, target) -> do
    notVisited <- H.insert visited target
    when notVisited $ f target

traverseSources :: (Setup s) => Node s -> (Node s -> IO ()) -> IO ()
traverseSources target = H.traverse (refsSources $ HR.value $ target)

getStats :: (Setup s) => Node s -> IO (Int, Int, Int)
getStats root = do
  nodesCounter <- newIORef 0
  edgesCounter <- newIORef 0
  indexesCounter <- newIORef 0
  knownSet :: H.Set H.Basic (Node s) <- H.new
  loopQueue <- newIORef []
  let 
    loop = do
      dequeue >>= \case
        Nothing -> return ()
        Just node -> do
          modifyIORef nodesCounter succ
          do
            targetsSet :: H.Set H.Basic (Node s) <- H.new
            H.traverse (refsTargets $ HR.value $ node) $ \(i, target) -> do
              notVisited <- H.insert targetsSet target
              when notVisited $ do
                modifyIORef edgesCounter succ
                enqueue target
              modifyIORef indexesCounter succ
          loop
    dequeue = do
      readIORef loopQueue >>= \case
        h : t -> writeIORef loopQueue t >> return (Just h)
        [] -> return Nothing
    enqueue node = do
      H.elem knownSet node >>= \case
        True -> return ()
        False -> do
          H.insertFast knownSet node
          modifyIORef loopQueue (node:)

  enqueue root
  loop

  (,,) <$> readIORef nodesCounter <*> readIORef edgesCounter <*> readIORef indexesCounter


-- ** Higher level operations
-------------------------

remove :: (Setup s) => Node s -> IO ()
remove node = traverseSources node $ \s -> removeTarget s node

getTargetsByIndex :: (Setup s) => Node s -> Index s -> IO [Node s]
getTargetsByIndex n i = do
  l <- newIORef []
  traverseTargetsByIndex n i $ \t -> modifyIORef l (t:)
  readIORef l

getSources :: (Setup s) => Node s -> IO [Node s]
getSources n = do
  l <- newIORef []
  traverseSources n $ \s -> modifyIORef l (s:)
  readIORef l


-- * Serialization
-------------------------

instance (Setup s) => Serializable IO (Node s) where

  serialize root = do
    loopQueue <- lift $ newIORef []
    knownSet :: H.Set H.Basic (Node s) <- lift $ H.new
    indexesMap :: H.Map H.Basic (Node s) Int <- lift $ H.new
    indexCounter <- lift $ newIORef 0
    let 
      loop = do
        dequeue >>= \case
          Nothing -> return ()
          Just node -> do
            (count, serializeTargets) <- liftIO $ do
              count <- newIORef (0 :: Int)
              serializeTargets <- newIORef (return ())
              traverseTargets node $ \target -> do
                modifyIORef count $ succ
                modifyIORef serializeTargets $ \acc -> do
                  acc
                  serializeValue target
                  enqueue target
              (,) <$> readIORef count <*> readIORef serializeTargets
            serialize count
            serializeTargets
            loop
      serializeValue node = do
        (liftIO $ H.lookup indexesMap node) >>= \case
          Just i -> do
            serialize True
            serialize i
          Nothing -> do
            serialize False
            do
              i <- liftIO $ readIORef indexCounter <* modifyIORef indexCounter succ
              liftIO $ H.insertFast indexesMap (node, i)
            serialize =<< do liftIO $ getValue node
      enqueue node = liftIO $ do
        H.elem knownSet node >>= \case
          True -> return ()
          False -> do
            H.insertFast knownSet node
            modifyIORef loopQueue (node:)
      dequeue = liftIO $ do
        readIORef loopQueue >>= \case
          h : t -> do
            writeIORef loopQueue t
            return $ Just h
          [] -> return Nothing
    serializeValue root
    enqueue root
    loop

  deserialize = do
    indexedNodes <- liftIO $ V.newSized $ 10^6
    unpopulatedNodes <- liftIO $ newIORef []
    let
      fetchUnpopulatedNode = 
        liftIO $ atomicModifyIORef' unpopulatedNodes $ \case
          head : tail -> (tail, Just head)
          _ -> ([], Nothing)
      enqueueUnpopulatedNode node = 
        liftIO $ modifyIORef unpopulatedNodes (node:)
      deserializeNode = do
        deserialize >>= \case
          True -> liftIO . V.unsafeLookup indexedNodes =<< deserialize
          False -> do
            newNode <- liftIO . new =<< deserialize
            liftIO $ V.append indexedNodes newNode
            enqueueUnpopulatedNode newNode
            return newNode
      loopAddTargets = do
        fetchUnpopulatedNode >>= \case
          Nothing -> return ()
          Just source -> do
            count <- deserialize
            replicateM_ count $ do
              target <- deserializeNode
              liftIO $ addTarget source target
            loopAddTargets

    node <- deserializeNode
    loopAddTargets
    
    return node



