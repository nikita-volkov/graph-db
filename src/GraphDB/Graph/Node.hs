module GraphDB.Graph.Node where

import GraphDB.Prelude
import qualified Data.HashTable.IO as Table
import qualified GraphDB.DIOVector as DIOVector; import GraphDB.DIOVector (DIOVector)


data Node n e = Node { 
  valueRef :: IORef n,
  edgesTable :: Table.BasicHashTable e [Node n e]
}

instance Eq (Node n e) where
  a == b = valueRef a == valueRef b

insertEdge :: (Hashable e, Eq e) => Node n e -> e -> Node n e -> IO ()
insertEdge (Node _ table) edge target =
  Table.lookup table edge >>=
  return . fromMaybe [] >>=
  return . (target:) >>=
  Table.insert table edge

deleteEdges :: (Hashable e, Eq e) => Node n e -> e -> IO ()
deleteEdges source edge = Table.delete (edgesTable source) edge

deleteEdge :: (Hashable e, Eq e) => Node n e -> e -> Node n e -> IO ()
deleteEdge source edge target =
  Table.lookup table edge >>=
  return . fromMaybe [] >>=
  return . delete target >>=
  \row -> if null row
    then Table.delete table edge
    else Table.insert table edge row
  where
    table = edgesTable source

new :: n -> IO (Node n e)
new value = Node <$> newIORef value <*> Table.new

getValue :: Node n e -> IO n
getValue (Node ref _) = readIORef ref

setValue :: Node n e -> n -> IO ()
setValue (Node valueRef _) value = writeIORef valueRef value

getTargets :: (Hashable e, Eq e) => Node n e -> e -> IO [Node n e]
getTargets (Node _ edgesTable) edge = fromMaybe [] <$> Table.lookup edgesTable edge

foldEdgesM :: Node n e -> z -> (z -> (e, Node n e) -> IO z) -> IO z
foldEdgesM node z zToEdgeNodeToIOZ = Table.foldM zToEdgeNodeListToIOZ z $ edgesTable node
  where
    zToEdgeNodeListToIOZ z (edge, nodeList) = foldM zToEdgeNodeToIOZ z $ map (edge,) nodeList


instance (Serializable IO n, Serializable IO e, Hashable e, Eq e) => Serializable IO (Node n e) where
  serialize node = do
    untraversedNodesQueue <- liftIO $ atomically $ newTQueue
    registryVar <- liftIO $ atomically $ newTVar ([], 0)
    go untraversedNodesQueue registryVar
    where
      go untraversedNodesQueue registryVar = serializeNodeValue node >> loopTraverse
        where
          serializeNodeValue node = serializeValue >> updateState 
            where
              serializeValue = do
                indexM <- lookupSerializedNodeIndex node
                case indexM of
                  Nothing -> serialize False >> (liftIO $ getValue node) >>= serialize
                  Just i -> serialize True >> serialize i
              updateState = liftIO $ atomically $ writeTQueue untraversedNodesQueue node
          loopTraverse = do
            nodeM <- liftIO $ atomically $ tryReadTQueue untraversedNodesQueue
            case nodeM of
              Nothing -> return ()
              Just node -> do
                (edgesCount, serializeEdges) <- 
                  liftIO $ foldEdgesM node (0 :: Int, return ()) $ 
                    \(count, serializeAcc) (edge, target) -> do
                      return (succ count, serializeAcc >> serialize edge >> serializeNodeValue target)
                serialize edgesCount
                serializeEdges
                loopTraverse
          lookupSerializedNodeIndex node = do
            (list, length) <- liftIO $ atomically $ readTVar registryVar
            return $ (length-) <$> elemIndex node list
  deserialize = do
    nodesRegistry <- liftIO $ DIOVector.new
    untraversedNodesQueue <- liftIO $ newTQueueIO
    go nodesRegistry untraversedNodesQueue
    where
      go nodesRegistry untraversedNodesQueue = deserializeNode <* loopTraverse
        where
          deserializeNode = do
            node <- do
              exists <- deserialize
              if exists
                then do
                  index <- deserialize
                  liftIO $ DIOVector.unsafeLookup nodesRegistry index
                else liftIO . new =<< deserialize
            liftIO $ DIOVector.append nodesRegistry node
            liftIO $ atomically $ writeTQueue untraversedNodesQueue node
            return node
          loopTraverse = do
            nodeM <- liftIO $ atomically $ tryReadTQueue untraversedNodesQueue
            case nodeM of
              Nothing -> return ()
              Just node -> traverseNode node >> loopTraverse
            where
              traverseNode node = do
                edgesCount <- deserialize
                replicateM_ edgesCount $ do
                  edge <- deserialize
                  tardeserializeNode <- deserializeNode
                  liftIO $ insertEdge node edge tardeserializeNode

