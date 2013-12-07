module GraphDB.Engine.Node where

import GraphDB.Prelude hiding (Any, traverse)
import GHC.Exts (Any)
import qualified GraphDB.SNMultiTable as MT
import qualified GraphDB.DIOVector as DIOVector; import GraphDB.DIOVector (DIOVector)
import qualified GraphDB.IOStableNameSet as IOStableNameSet; import GraphDB.IOStableNameSet (IOStableNameSet)
import qualified Data.HashTable.IO as HashTables


type MT = MT.SNMultiTable

-- |
-- A public representation which all functions revolve around.
-- 
data Node v =
  Node
    !(Value_Type v)
    !(Refs v)

-- |
-- An internally used data structure for memory-efficient storage.
data Refs v =
  Refs
    {-# UNPACK #-}
    !(IORef Any)
    -- | Targets by type.
    {-# UNPACK #-} 
    !(MT (Value_Type v) (Refs v))
    -- | Targets by index.
    {-# UNPACK #-}
    !(MT (Value_Index v) (Refs v))
    -- | Source refs.
    {-# UNPACK #-}
    !(MT (Value_Type v) (Refs v))

-------------
-- Accessors.
-------------

valueType (Node z _) = z
refs (Node _ z) = z
valueRef (Node _ (Refs z _ _ _)) = z
targetsByType (Node _ (Refs _ z _ _)) = z
targetsByIndex (Node _ (Refs _ _ z _)) = z
sourcesByType (Node _ (Refs _ _ _ z)) = z

-------------

class 
  (MT.Key (Value_Index v), MT.Key (Value_Type v), Serializable IO v,
   Index (Value_Index v), Value_Type v ~ Index_Type (Value_Index v)) => 
  Value v 
  where
    type Value_Index v
    type Value_Type v
    value_indexes :: Value_Type v -> v -> [Value_Index v]
    value_decompose :: v -> (Value_Type v, Any)
    value_compose :: Value_Type v -> Any -> v

class Index i where
  type Index_Type i
  index_targetType :: i -> Index_Type i

-------------

new :: Value v => v -> IO (Node v)
new uv = Node <$> pure t <*> (Refs <$> newIORef v <*> MT.new <*> MT.new <*> MT.new)
  where
    (t, v) = value_decompose uv

getValue :: Value v => Node v -> IO v
getValue (Node t (Refs ref _ _ _)) = value_compose <$> pure t <*> readIORef ref

setValue :: Value v => Node v -> v -> IO ()
setValue node@(Node t refs@(Refs valueRef _ _ sourcesByType)) newValue = 
  if newValue_Type /= t 
    then error "Attempt to set a value of a wrong type"
    else do
      oldValue <- getValue node
      MT.traverse sourcesByType $ \sourceType ->
        let
          oldIndexes = value_indexes sourceType oldValue
          newIndexes = value_indexes sourceType newValue
          in \targetRefs@(Refs _ _ targetTargetsByIndex _) -> do
            forM_ oldIndexes $ \i -> 
              MT.delete targetTargetsByIndex (i, refs) >>= \case
                True -> return ()
                False -> _error "Old index not found"
            forM_ newIndexes $ \i -> 
              MT.insert targetTargetsByIndex (i, refs) >>= \case
                True -> return ()
                False -> _error "New index collision"
      writeIORef valueRef newValueAny
  where
    (newValue_Type, newValueAny) = value_decompose newValue
    _error = error . ("GraphDB.Engine.Node.setValue: " ++)

getSourcesByType :: Value v => Node v -> Value_Type v -> IO [Node v]
getSourcesByType (Node _ (Refs _ _ _ sourceRefsTable)) t =
  map (Node t) <$> MT.lookupByKey sourceRefsTable t

getTargetsByType :: Value v => Node v -> Value_Type v -> IO [Node v]
getTargetsByType (Node _ (Refs _ table _ _)) t =
  map (Node t) <$> MT.lookupByKey table t

getTargetsByIndex :: Value v => Node v -> Value_Index v -> IO [Node v]
getTargetsByIndex (Node _ (Refs _ _ table _)) index =
  map (Node (index_targetType index)) <$> MT.lookupByKey table index

addTarget :: Value v => Node v -> Node v -> IO Bool
addTarget source target = do
  updateTarget >>= \case
    False -> return False
    True -> updateSource >> return True
  where
    _error = error . ("GraphDB.Engine.Node.addTarget: " ++)
    updateSource = do
      targetIndexes <- value_indexes <$> pure (valueType source) <*> getValue target
      forM_ targetIndexes $ \i -> 
        MT.insert (targetsByIndex source) (i, refs target) >>= \case
          True -> return ()
          False -> _error "Node already exists under provided index"
      MT.insert (targetsByType source) (valueType target, refs target) >>= \case
        True -> return True
        False -> _error "Node already exists under provided type"
    updateTarget = do
      MT.insert (sourcesByType target) (valueType source, refs source)

removeTarget :: Value v => Node v -> Node v -> IO Bool
removeTarget source target = do
  updateTarget >>= \case
    False -> return False
    True -> do
      updateSource
      maintain target
      return True
  where
    _error = error . ("GraphDB.Engine.Node.removeTarget: " ++)
    updateTarget = MT.delete (sourcesByType target) (valueType source, refs source)
    updateSource = do
      indexes <- value_indexes <$> pure (valueType source) <*> getValue target
      forM_ indexes $ \i ->
        MT.delete (targetsByIndex source) (i, refs target) >>= \case
          True -> return ()
          False -> _error "Target not found by index"
      MT.delete (targetsByType source) (valueType target, refs target) >>= \case
        True -> return ()
        False -> _error "Target not found by type"

-- |
-- If after being removed the target node has no edges to it left, 
-- it becomes unreachable. It however will not get garbage-collected
-- if it itself retains edges to other nodes, 
-- because this leaves back-references to it in them. 
-- That's why in this case we must delete all outgoing edges from it manually.
maintain :: Value v => Node v -> IO ()
maintain node = do
  MT.getNull (sourcesByType node) >>= \case
    True -> return ()
    False -> traverseTargets node $ removeTarget node >=> \case
      True -> return ()
      False -> _error "Target removal failed"
  where
    _error = error . ("GraphDB.Engine.Node.maintain: " ++)

foldTargets :: Node v -> z -> (z -> Node v -> IO z) -> IO z
foldTargets node z f = MT.foldM (targetsByType node) z $ \z t refs -> f z (Node t refs)

traverseTargets :: Node v -> (Node v -> IO ()) -> IO ()
traverseTargets node action = foldTargets node () $ \() -> action

foldSources :: Node v -> z -> (z -> Node v -> IO z) -> IO z
foldSources node z f = MT.foldM (sourcesByType node) z $ \z t refs -> f z (Node t refs)

traverseSources :: Node v -> (Node v -> IO ()) -> IO ()
traverseSources node action = foldSources node () $ \() -> action

traverse :: Node v -> (Node v -> IO (Node v -> IO ())) -> IO ()
traverse root f = do
  knownRefs <- HashTables.new 
            :: IO (HashTables.BasicHashTable (StableName (Refs t)) ())
  unvisitedNodes <- newIORef []
  let
    dequeue =
      readIORef unvisitedNodes >>= \case
        head : tail -> do
          writeIORef unvisitedNodes tail
          return $ Just head
        [] -> return Nothing
    enqueue node@(Node _ refs) = do
      sn <- makeStableName refs
      HashTables.lookup knownRefs sn >>= \case
        Just () -> return ()
        Nothing -> do
          HashTables.insert knownRefs sn ()
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

getStats :: Node v -> IO (Int, Int)
getStats root = do
  nodesCounter <- newIORef 0
  edgesCounter <- newIORef 0
  traverse root $ 
    const $ do
      modifyIORef nodesCounter succ
      return $ const $ modifyIORef edgesCounter succ 
  (,) <$> readIORef nodesCounter <*> readIORef edgesCounter

-------------

instance Eq (Node v) where
  n == n' = valueRef n == valueRef n'

instance (Value v) => Serializable IO (Node v) where

  serialize root = do
    knownRefs
      :: HashTables.BasicHashTable (StableName (Refs t)) ()
      <- liftIO $ HashTables.new
    unvisitedNodes <- liftIO $ newIORef []
    indexTable 
      :: HashTables.BasicHashTable (StableName (Refs t)) Int
      <- liftIO $ HashTables.new
    indexCounter <- liftIO $ newIORef 0

    let
      dequeueNode = do
        readIORef unvisitedNodes >>= \case
          head : tail -> do
            writeIORef unvisitedNodes tail
            return $ Just head
          [] -> return Nothing
      enqueueNode node@(Node _ refs) = do
        sn <- makeStableName refs
        HashTables.lookup knownRefs sn >>= \case
          Just () -> return ()
          Nothing -> do
            HashTables.insert knownRefs sn ()
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
      serializeNodeRef node@(Node _ refs) = do
        sn <- liftIO $ makeStableName refs
        (liftIO $ HashTables.lookup indexTable sn) >>= \case
          Just i -> do
            serialize True
            serialize i
            return i
          Nothing -> do
            serialize False
            serialize =<< (liftIO $ getValue node)
            i <- liftIO $ readIORef indexCounter
            liftIO $ HashTables.insert indexTable sn i
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
              liftIO $ addTarget target source
            loopAddTargets

    node <- deserializeNode
    loopAddTargets
    
    return node
