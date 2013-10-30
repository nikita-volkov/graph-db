module TPM.GraphDB.Graph.Node where

import TPM.GraphDB.Prelude
import qualified Data.HashTable.IO as Table
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal
import qualified Acid.IO.PutT as PutT; import Acid.IO.PutT (PutT)
import qualified Acid.IO.GetT as GetT; import Acid.IO.GetT (GetT)
import qualified Acid.IO.SerializeM as SerializeM
import qualified TPM.GraphDB.DIOVector as DIOVector; import TPM.GraphDB.DIOVector (DIOVector)


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

deleteEdge :: (Hashable e, Eq e) => Node n e -> e -> Node n e -> IO ()
deleteEdge source edge target = Table.delete (edgesTable source) edge

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


instance (SafeCopy n, SafeCopy e, Hashable e, Eq e) => SerializeM.SerializeM (Node n e) IO where
  putT node = do
    untraversedNodesQueue <- liftIO $ atomically $ newTQueue
    registryVar <- liftIO $ atomically $ newTVar ([], 0)
    go untraversedNodesQueue registryVar
    where
      go untraversedNodesQueue registryVar = putNodeValue node >> loopTraverse
        where
          putNodeValue node = putValue >> updateState 
            where
              putValue = do
                indexM <- lookupSerializedNodeIndex node
                case indexM of
                  Nothing -> put False >> (liftIO $ getValue node) >>= safePut
                  Just i -> put True >> put i
              updateState = liftIO $ atomically $ writeTQueue untraversedNodesQueue node
          loopTraverse = do
            nodeM <- liftIO $ atomically $ tryReadTQueue untraversedNodesQueue
            case nodeM of
              Nothing -> return ()
              Just node -> do
                (edgesCount, putEdges) <- liftIO $ foldEdgesM node (0 :: Int, return ()) $ 
                  \ (count, put) (edge, target) -> do
                    return (succ count, put >> safePut edge >> putNodeValue target)
                put edgesCount
                putEdges
                loopTraverse
          lookupSerializedNodeIndex node = do
            (list, length) <- liftIO $ atomically $ readTVar registryVar
            return $ (length-) <$> elemIndex node list
      put a = PutT.liftPut $ Cereal.put a
      safePut a = PutT.liftPut $ SafeCopy.safePut a
  getT = do
    nodesRegistry <- liftIO $ DIOVector.new
    untraversedNodesQueue <- liftIO $ newTQueueIO
    go nodesRegistry untraversedNodesQueue
    where
      go nodesRegistry untraversedNodesQueue = getNode <* loopTraverse
        where
          getNode = do
            node <- do
              exists <- get
              if exists
                then do
                  index <- get
                  liftIO $ DIOVector.unsafeLookup nodesRegistry index
                else liftIO . new =<< safeGet
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
                edgesCount <- get
                replicateM_ edgesCount $ do
                  edge <- safeGet
                  targetNode <- getNode
                  liftIO $ insertEdge node edge targetNode
      get = GetT.liftGet $ Cereal.get
      safeGet = GetT.liftGet $ SafeCopy.safeGet

