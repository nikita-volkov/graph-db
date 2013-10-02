module TPM.GraphDB.Node.Deserialize where

import TPM.GraphDB.Prelude hiding (get)
import TPM.GraphDB.Node as Node
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal
import qualified TPM.GraphDB.DIOVector as DIOVector; import TPM.GraphDB.DIOVector (DIOVector)
import qualified TPM.GraphDB.GetT as GetT; import TPM.GraphDB.GetT (GetT)



run :: 
  (SafeCopy (Value db), SafeCopy (Edge db), Hashable (Edge db), Eq (Edge db)) => 
  ByteString -> IO (Node db)
run bs = do
  state <- (,) <$> DIOVector.new <*> newTChanIO
  r <- flip GetT.run bs $ flip runReaderT state $ do
    node <- getUntraversedNode
    loopTraverseNode
    return node
  case r of
    GetT.Fail msg -> error $ "Parser failed: " ++ msg
    GetT.Partial _ -> error "Partial result"
    GetT.Done node "" -> return node
    GetT.Done _ _ -> error "Parser didn't consume the whole input"



type Deserialize db = ReaderT (DeserializeState db) (GetT IO)

-- | Deserialized nodes by indexes and a queue of untraversed nodes.
type DeserializeState db = (DIOVector (Node db), TChan (Node db))



get :: (Cereal.Serialize a) => Deserialize db a
get = lift $ GetT.liftGet $ Cereal.get

safeGet :: (SafeCopy a) => Deserialize db a
safeGet = lift $ GetT.liftGet $ SafeCopy.safeGet

loopTraverseNode :: 
  (SafeCopy (Edge db), SafeCopy (Value db), Hashable (Edge db), Eq (Edge db)) => 
  Deserialize db ()
loopTraverseNode = do
  nodeM <- do
    (_, chan) <- ask
    liftIO $ atomically $ do
      empty <- isEmptyTChan chan
      if empty
        then return Nothing
        else Just <$> readTChan chan
  case nodeM of
    Nothing -> return ()
    Just node -> do
      traverseNode node
      loopTraverseNode

getUntraversedNode :: (SafeCopy (Value db)) => Deserialize db (Node db)
getUntraversedNode = do
  node <- do
    value <- safeGet
    liftIO $ Node.new value
  do
    (nodesRegistry, untraversedNodes) <- ask
    liftIO $ DIOVector.append nodesRegistry node
    liftIO $ atomically $ writeTChan untraversedNodes node
  return node

traverseNode :: 
  (SafeCopy (Edge db), SafeCopy (Value db), Hashable (Edge db), Eq (Edge db)) => 
  Node db -> Deserialize db ()
traverseNode node = do
  edgesCount <- get
  replicateM_ edgesCount $ do
    edge <- safeGet
    target <- do
      exists <- get
      if exists
        then do
          index <- get
          (nodesRegistry, untraversedNodes) <- ask
          liftIO $ DIOVector.unsafeLookup nodesRegistry index
        else do
          getUntraversedNode
    liftIO $ Node.insertEdge node edge target




