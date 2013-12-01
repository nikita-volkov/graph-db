-- |
-- An API for non-static use-cases, such as serialization.
module GraphDB.Graph.DynamicNode where

import GraphDB.Prelude
import GraphDB.Graph.Types
import qualified GraphDB.Graph.Node as Node; import GraphDB.Graph.Node (Node)
import qualified GraphDB.IOStableNameSet as IOStableNameSet
import qualified GraphDB.DIOVector as DIOVector; import GraphDB.DIOVector (DIOVector)
import qualified Data.HashTable.IO as HashTables

newtype DynamicNode t = DynamicNode (UnionValueType t, Node t)

new :: (GraphTag t) => UnionValue t -> IO (DynamicNode t)
new uv = DynamicNode <$> ((,) <$> (pure $ unionValueType uv) <*> (Node.new $ unionValueAny uv))

getValue :: (GraphTag t) => DynamicNode t -> IO (UnionValue t)
getValue (DynamicNode (t, n)) = do
  any <- Node.getValue n
  return $ unionValueTypeValue any t

addTarget :: GraphTag t => DynamicNode t -> DynamicNode t -> IO ()
addTarget target@(DynamicNode (targetUVT, targetNode)) (DynamicNode (sourceUVT, sourceNode)) = do
  targetIndexHashes <- unionValueIndexHashes <$> pure sourceUVT <*> getValue target
  Node.addTarget (targetNode, targetUVT, targetIndexHashes) sourceNode

forMTargets_ :: DynamicNode t -> (DynamicNode t -> IO ()) -> IO ()
forMTargets_ (DynamicNode (t, n)) f = Node.forMTargets_ n $ \tn -> f (DynamicNode tn)

foldTargets :: DynamicNode t -> z -> (z -> DynamicNode t -> IO z) -> IO z
foldTargets (DynamicNode (t, n)) z f = Node.foldTargets n z $ \z tn -> f z (DynamicNode tn)

-- |
-- Traverse all nodes.
forMAllNodes_ :: DynamicNode t -> (DynamicNode t -> IO ()) -> IO ()
forMAllNodes_ n f = do
  visitedNodes <- IOStableNameSet.new
  let
    visitNode dn = do
      f dn
      forMTargets_ dn $ \target@(DynamicNode (_, n)) -> do
        IOStableNameSet.lookup visitedNodes n >>= \case
          True -> return ()
          False -> do
            IOStableNameSet.insert visitedNodes n
            visitNode target
  visitNode n


instance (GraphTag t) => Serializable IO (DynamicNode t) where
  
  serialize root = do
    traversalQueue <- liftIO $ newIORef []
    indexTable 
      :: HashTables.BasicHashTable (StableName (DynamicNode t)) Int
      <- liftIO $ HashTables.new
    indexTableSizeRef <- liftIO $ newIORef 0
    traversedNodes <- liftIO $ IOStableNameSet.new
    let 
      loop = do
        fetchFromTraversalQueue >>= \case
          Nothing -> return ()
          Just node -> do
            (count, serializeTargets) <- 
              liftIO $ foldTargets node (0 :: Int, return ()) $ 
                \(count, serializeTargets) target -> let 
                  serializeTargets' = do
                    serializeTargets
                    serializeNodeRef target
                    (liftIO $ IOStableNameSet.lookup traversedNodes target) >>= \case
                      False -> insertToTraversalQueue target
                      _ -> return ()
                  in return (succ count, serializeTargets')
            serialize count
            serializeTargets
            liftIO $ IOStableNameSet.insert traversedNodes node
            loop
      fetchFromTraversalQueue = liftIO $
        readIORef traversalQueue >>= \case
          head : tail -> writeIORef traversalQueue tail >> return (Just head)
          [] -> return Nothing
      insertToTraversalQueue node = 
        liftIO $ modifyIORef traversalQueue (node:)
      serializeNodeRef node = do
        sn <- liftIO $ makeStableName node
        (liftIO $ HashTables.lookup indexTable sn) >>= \case
          Just i -> do
            serialize True
            serialize i
            return i
          Nothing -> do
            serialize False
            serialize =<< (liftIO $ getValue node)
            i <- liftIO $ readIORef indexTableSizeRef
            liftIO $ HashTables.insert indexTable sn i
            liftIO $ writeIORef indexTableSizeRef (succ i)
            return i

    serializeNodeRef root
    insertToTraversalQueue root
    loop

  deserialize = do
    indexedNodes <- liftIO $ DIOVector.new
    unpopulatedNodes <- liftIO $ newIORef []
    let
      fetchUnpopulatedNode = liftIO $ readIORef unpopulatedNodes >>= headZ
      enqueueUnpopulatedNode node = liftIO $ modifyIORef unpopulatedNodes (node:)
      deserializeNode = do
        deserialize >>= \case
          True -> liftIO . DIOVector.unsafeLookup indexedNodes =<< deserialize
          False -> do
            newNode <- liftIO . new =<< deserialize
            liftIO $ DIOVector.append indexedNodes newNode
            enqueueUnpopulatedNode newNode
            return newNode
      loopAddTargets = do
        source <- fetchUnpopulatedNode
        count <- deserialize
        replicateM count $ do
          target <- deserializeNode
          liftIO $ addTarget target source

    node <- deserializeNode
    loopAddTargets
    
    return node
