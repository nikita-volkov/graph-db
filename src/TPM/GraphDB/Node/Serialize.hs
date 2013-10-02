module TPM.GraphDB.Node.Serialize where

import TPM.GraphDB.Prelude hiding (put)
import TPM.GraphDB.Node as Node
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal



-- |
-- A single serializer on a root node.
-- Manages a shared registry of all nodes, thus elluding repeated serialization of same data.
-- 
-- Strategy A:
-- 1. Put a dictionary of all nodes by refs.
-- 
-- Strategy B:
-- 1. Put a node's value.
-- 2. Put all its edges with refs to others nodes.
-- 3. Traverse all referred nodes by going to step 1 with each of them.
-- 
-- FIXME: accumulating 'Cereal.Put' probably is very inefficient.
run :: (SafeCopy (Value db), SafeCopy (Edge db)) => Node db -> IO Cereal.Put
run root = do
  state <- (,) <$> newIORef ([], 0) <*> newTChanIO
  execWriterT $ flip runReaderT state $ do
    putNodeValue root
    loopTraverseNode



type Serialize db = ReaderT (SerializeState db) (WriterT Cereal.Put IO)

-- | A list of serialized nodes, its length and a queue of untraversed nodes.
type SerializeState db = (IORef ([Node db], Int), TChan (Node db))



put :: (Cereal.Serialize a) => a -> Serialize db ()
put a = tell $ Cereal.put a

safePut :: (SafeCopy a) => a -> Serialize db ()
safePut a = tell $ SafeCopy.safePut a

putNodeValue :: (SafeCopy (Value db)) => Node db -> Serialize db ()
putNodeValue node = putValue >> updateState where
  putValue = do
    indexM <- lookupSerializedNodeIndex node
    case indexM of
      Nothing -> put False >> (liftIO $ Node.getValue node) >>= safePut
      Just i -> put True >> put i
  updateState = do
    (_, untraversedNodes) <- ask
    liftIO $ atomically $ writeTChan untraversedNodes node

loopTraverseNode :: (SafeCopy (Value db), SafeCopy (Edge db)) => Serialize db ()
loopTraverseNode = do
  nodeM <- do
    (_, chan) <- ask
    liftIO $ atomically $ tryReadTChan chan
  case nodeM of
    Nothing -> return ()
    Just node -> do
      traverseNode node
      loopTraverseNode

traverseNode :: (SafeCopy (Value db), SafeCopy (Edge db)) => Node db -> Serialize db ()
traverseNode node = do
  (edgesCount, putEdges) <- 
    liftIO $ Node.foldEdgesM node (0 :: Int, return ()) $ \(count, put) (edge, target) -> do
      return (succ count, put >> safePut edge >> putNodeValue target)
  put edgesCount
  putEdges

lookupSerializedNodeIndex :: Node db -> Serialize db (Maybe Int)
lookupSerializedNodeIndex node = do
  (registryRef, _) <- ask
  (list, length) <- liftIO $ readIORef registryRef
  return $ (length-) <$> elemIndex node list


