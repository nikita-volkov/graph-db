module GraphDB.Graph where

import GraphDB.Util.Prelude hiding (Any, traverse)
import GHC.Exts (Any)
import qualified GraphDB.Util.DIOVector as DIOVector; import GraphDB.Util.DIOVector (DIOVector)
import qualified HashtablesPlus as HP
import qualified HashtablesPlus.HashRef as HashRef


type MT k v = HP.MultiTable HP.Linear k (HP.HashRefSet HP.Linear v)
type SizedMT k v = HP.Sized (MT k v)

-- |
-- A public representation which all functions revolve around.
-- 
data Node t = 
  Node {
    nodeValueType :: !t,
    nodeRefs :: {-# UNPACK #-} !(HashRef.HashRef (Refs t))
  }

-- |
-- An internally used data structure for memory-efficient storage.
data Refs t =
  Refs {
    refsValueRef :: {-# UNPACK #-} !(IORef Any),
    -- Targets by type.
    refsTargetsByType :: {-# UNPACK #-} !(MT t (Refs t)),
    -- Targets by index.
    refsTargetsByIndex :: {-# UNPACK #-} !(MT (Index t) (Refs t)),
    -- Source refs.
    refsSourcesByType :: {-# UNPACK #-} !(SizedMT t (Refs t))
  }

-- Accessors
-------------------------

nodeValueRef = refsValueRef . HashRef.value . nodeRefs
nodeTargetsByType = refsTargetsByType . HashRef.value . nodeRefs
nodeTargetsByIndex = refsTargetsByIndex . HashRef.value . nodeRefs
nodeSourcesByType = refsSourcesByType . HashRef.value . nodeRefs

-------------

class 
  (HP.Key (Index t), HP.Key t, Serializable IO (Value t)) => 
  Type t 
  where
    type Index t
    type Value t
    indexes :: Value t -> t -> [Index t]
    decomposeValue :: Value t -> (t, Any)
    composeValue :: t -> Any -> Value t
    targetType :: Index t -> t

-------------

new :: Type t => Value t -> IO (Node t)
new uv = do
  refs <- Refs <$> newIORef v <*> HP.new <*> HP.new <*> HP.new
  refsHR <- HashRef.new refs
  return $ Node t refsHR
  where
    (t, v) = decomposeValue uv

getValue :: Type t => Node t -> IO (Value t)
getValue n = 
  composeValue <$> 
    pure (nodeValueType n) 
      <*> 
    readIORef (nodeValueRef n)

setValue :: Type t => Node t -> Value t -> IO ()
setValue node newValue = 
  if newValueType /= nodeValueType node 
    then error "Attempt to set a value of a wrong type"
    else do
      oldValue <- getValue node
      HP.forM_ (nodeSourcesByType node) $ \(sourceType, targetRefsHR) ->
        let
          oldIndexes = indexes oldValue sourceType
          newIndexes = indexes newValue sourceType
          targetRefs = HashRef.value targetRefsHR
          targetTargetsByIndex = refsTargetsByIndex targetRefs
          in do
            forM_ oldIndexes $ \i -> 
              HP.delete targetTargetsByIndex (i, (nodeRefs node)) >>= \case
                True -> return ()
                False -> $bug "Old index not found"
            forM_ newIndexes $ \i -> 
              HP.insert targetTargetsByIndex (i, (nodeRefs node)) >>= \case
                True -> return ()
                False -> $bug "New index collision"
      writeIORef (nodeValueRef node) newValueAny
  where
    (newValueType, newValueAny) = decomposeValue newValue

getSourcesByType :: Type t => Node t -> t -> IO [Node t]
getSourcesByType (Node _ (HashRef.HashRef _ (Refs _ _ _ sourceRefsTable))) t =
  map (Node t) <$> HP.lookupMulti sourceRefsTable t

getTargetsByType :: Type t => Node t -> t -> IO [Node t]
getTargetsByType (Node _ (HashRef.HashRef _ (Refs _ table _ _))) t =
  map (Node t) <$> HP.lookupMulti table t

getTargetsByIndex :: Type t => Node t -> Index t -> IO [Node t]
getTargetsByIndex (Node _ (HashRef.HashRef _ (Refs _ _ table _))) index =
  map (Node (targetType index)) <$> HP.lookupMulti table index

addTarget :: Type t => Node t -> Node t -> IO Bool
addTarget source target = do
  updateTarget >>= \case
    False -> return False
    True -> updateSource >> return True
  where
    updateSource = do
      targetIndexes <- indexes <$> getValue target <*> pure (nodeValueType source)
      forM_ targetIndexes $ \i -> 
        HP.insert (nodeTargetsByIndex source) (i, nodeRefs target) >>= \case
          True -> return ()
          False -> $bug "Node already exists under provided index"
      HP.insert (nodeTargetsByType source) (nodeValueType target, nodeRefs target) >>= \case
        True -> return True
        False -> $bug "Node already exists under provided type"
    updateTarget = do
      HP.insert (nodeSourcesByType target) (nodeValueType source, nodeRefs source)

removeTarget :: Type t => Node t -> Node t -> IO Bool
removeTarget source target = do
  updateTarget >>= \case
    False -> return False
    True -> do
      updateSource
      maintain target
      return True
  where
    updateTarget = HP.delete (nodeSourcesByType target) (nodeValueType source, nodeRefs source)
    updateSource = do
      indexes <- indexes <$> getValue target <*> pure (nodeValueType source)
      forM_ indexes $ \i ->
        HP.delete (nodeTargetsByIndex source) (i, nodeRefs target) >>= \case
          True -> return ()
          False -> $bug "Target not found by index"
      HP.delete (nodeTargetsByType source) (nodeValueType target, nodeRefs target) >>= \case
        True -> return ()
        False -> $bug "Target not found by type"

-- |
-- If after being removed the target node has no edges to it left, 
-- it becomes unreachable. It however will not get garbage-collected
-- if it itself retains edges to other nodes, 
-- because this leaves back-references to it in them. 
-- That's why in this case we must delete all outgoing edges from it manually.
maintain :: Type t => Node t -> IO ()
maintain node = do
  HP.null (nodeSourcesByType node) >>= \case
    True -> return ()
    False -> traverseTargets node $ void . removeTarget node

remove :: Type t => Node t -> IO ()
remove node = do
  traverseSources node $ \s -> void $ removeTarget s node

foldTargets :: (HP.Key t) => Node t -> z -> (z -> Node t -> IO z) -> IO z
foldTargets node z f = HP.foldM (nodeTargetsByType node) z $ \z (t, refs) -> f z (Node t refs)

traverseTargets :: (HP.Key t) => Node t -> (Node t -> IO ()) -> IO ()
traverseTargets node action = foldTargets node () $ \() -> action

foldSources :: (HP.Key t) => Node t -> z -> (z -> Node t -> IO z) -> IO z
foldSources node z f = HP.foldM (nodeSourcesByType node) z $ \z (t, refs) -> f z (Node t refs)

traverseSources :: (HP.Key t) => Node t -> (Node t -> IO ()) -> IO ()
traverseSources node action = foldSources node () $ \() -> action

traverse :: (HP.Key t) => Node t -> (Node t -> IO (Node t -> IO ())) -> IO ()
traverse root f = do
  knownRefs :: HP.HashRefSet HP.Cuckoo (Refs t) <- HP.new
  unvisitedNodes <- newIORef []
  let
    dequeue =
      readIORef unvisitedNodes >>= \case
        head : tail -> do
          writeIORef unvisitedNodes tail
          return $ Just head
        [] -> return Nothing
    enqueue node@(Node _ refsHR) = do
      HP.elem knownRefs refsHR >>= \case
        True -> return ()
        False -> do
          HP.insert knownRefs refsHR
          modifyIORef unvisitedNodes (node:)
    loop =
      dequeue >>= \case
        Nothing -> return ()
        Just node -> do
          f' <- f node
          traverseTargets node $ \t -> do
            f' t
            enqueue t
          loop
  enqueue root
  loop

getStats :: (HP.Key t) => Node t -> IO (Int, Int)
getStats root = do
  nodesCounter <- newIORef 0
  edgesCounter <- newIORef 0
  traverse root $ 
    const $ do
      modifyIORef nodesCounter succ
      return $ const $ modifyIORef edgesCounter succ 
  (,) <$> readIORef nodesCounter <*> readIORef edgesCounter

-------------

instance Eq (Node t) where
  n == n' = nodeValueRef n == nodeValueRef n'

instance (Type t) => Serializable IO (Node t) where

  serialize root = do
    knownRefs
      :: HP.HashRefSet HP.Cuckoo (Refs t)
      <- liftIO $ HP.new
    unvisitedNodes <- liftIO $ newIORef []
    indexTable 
      :: HP.Table HP.Cuckoo (HashRef.HashRef (Refs t)) Int
      <- liftIO $ HP.new
    indexCounter <- liftIO $ newIORef 0

    let
      dequeueNode = do
        readIORef unvisitedNodes >>= \case
          head : tail -> do
            writeIORef unvisitedNodes tail
            return $ Just head
          [] -> return Nothing
      enqueueNode node@(Node _ refsHR) = do
        HP.elem knownRefs refsHR >>= \case
          True -> return ()
          False -> do
            HP.insert knownRefs refsHR
            modifyIORef unvisitedNodes (node:)
      loop = do
        liftIO dequeueNode >>= \case
          Nothing -> return ()
          Just node -> do
            (count, serializeTargets) <- 
              liftIO $ foldTargets node (0 :: Int, return ()) $ \(count, serializeTargets) target -> do
                let 
                  -- ACHTUNG: order matters here!
                  serializeTargets' = do
                    serializeTargets
                    void $ serializeNodeRef target
                    liftIO $ enqueueNode target
                return (succ count, serializeTargets')
            serialize count
            serializeTargets
            loop
      serializeNodeRef node@(Node _ refsHR) = do
        (liftIO $ HP.lookup indexTable refsHR) >>= \case
          Just i -> do
            serialize True
            serialize i
            return i
          Nothing -> do
            serialize False
            serialize =<< (liftIO $ getValue node)
            i <- liftIO $ readIORef indexCounter
            liftIO $ HP.insert indexTable (refsHR, i)
            liftIO $ writeIORef indexCounter (succ i)
            return i

    serializeNodeRef root
    liftIO $ enqueueNode root
    loop
    
  deserialize = do
    indexedNodes <- liftIO $ DIOVector.new
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
          True -> liftIO . DIOVector.unsafeLookup indexedNodes =<< deserialize
          False -> do
            newNode <- liftIO . new =<< deserialize
            liftIO $ DIOVector.append indexedNodes newNode
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
