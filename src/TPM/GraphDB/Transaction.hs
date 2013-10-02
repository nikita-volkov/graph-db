module TPM.GraphDB.Transaction where

import TPM.GraphDB.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.DB as DB; import TPM.GraphDB.DB (DB)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)
import qualified TPM.GraphDB.Transaction.NodeRefRegistry as NodeRefRegistry; import TPM.GraphDB.Transaction.NodeRefRegistry (NodeRefRegistry)
import qualified TPM.GraphDB.Transaction.NodeRef as NodeRef; import TPM.GraphDB.Transaction.NodeRef (NodeRef)



class Transaction t where
  run :: DB db -> (forall state. t db state a) -> IO a
  getDB :: t db state (DB db)
  getNodeRefRegistry :: t db state (NodeRefRegistry db)



-- |
-- A write and read transaction. Only a single write-transaction executes at a time.
-- 
-- Here the /s/ is a state-thread making the escape of node-refs from transaction
-- impossible. Much inspired by the realization of 'ST'.
-- 
newtype Write db s a = Write (DB db -> NodeRefRegistry db -> IO a)

instance Transaction Write where
  run db (Write dbToRegistryToIO) = Dispatcher.runWrite (DB.dispatcher db) io where 
    io = NodeRefRegistry.new >>= dbToRegistryToIO db
  getDB = Write $ \z _ -> return z
  getNodeRefRegistry = Write $ \_ z -> return z

instance MonadIO (Write db s) where
  liftIO io = Write $ \_ _ -> io

instance Monad (Write db s) where
  return a = Write $ \_ _ -> return a
  writeA >>= aToWriteB = Write dbToRegToIO where
    dbToRegToIO db reg = ioA >>= aToIOB where
      Write dbToRegToIOA = writeA
      ioA = dbToRegToIOA db reg
      aToIOB a = ioB where
        Write dbToRegToIOB = aToWriteB a
        ioB = dbToRegToIOB db reg

instance Applicative (Write db s) where
  pure = return
  (<*>) = ap

instance Functor (Write db s) where
  fmap = liftM



-- |
-- A read-only transaction. Gets executed concurrently.
-- 
-- Here the /s/ is a state-thread making the escape of node-refs from transaction
-- impossible. Much inspired by the realization of 'ST'.
-- 
newtype Read db s a = Read (DB db -> NodeRefRegistry db -> IO a)

instance Transaction Read where
  run db (Read dbToRegistryToIO) = Dispatcher.runRead (DB.dispatcher db) io where 
    io = NodeRefRegistry.new >>= dbToRegistryToIO db
  getDB = Read $ \z _ -> return z
  getNodeRefRegistry = Read $ \_ z -> return z

instance MonadIO (Read db s) where
  liftIO io = Read $ \_ _ -> io

instance Monad (Read db s) where
  return a = Read $ \_ _ -> return a
  readA >>= aToReadB = Read dbToRegToIO where
    dbToRegToIO db reg = ioA >>= aToIOB where
      Read dbToRegToIOA = readA
      ioA = dbToRegToIOA db reg
      aToIOB a = ioB where
        Read dbToRegToIOB = aToReadB a
        ioB = dbToRegToIOB db reg

instance Applicative (Read db s) where
  pure = return
  (<*>) = ap

instance Functor (Read db s) where
  fmap = liftM



getRoot :: (Transaction t, Monad (t db s), MonadIO (t db s)) => t db s (NodeRef db s)
getRoot = do
  root <- getDB >>= return . DB.root
  registry <- getNodeRefRegistry
  liftIO $ NodeRefRegistry.newNodeRef registry root

newNode :: Node.Value db -> Write db s (NodeRef db s)
newNode value = do
  registry <- getNodeRefRegistry
  liftIO $ do
    node <- Node.new value
    NodeRefRegistry.newNodeRef registry node

getTargets :: ( Transaction t, Monad (t db s), MonadIO (t db s), Hashable (Node.Edge db), Eq (Node.Edge db) ) => 
              Node.Edge db -> NodeRef db s -> t db s [NodeRef db s]
getTargets edge refA = do
  registry <- getNodeRefRegistry
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodesB <- Node.getTargets nodeA edge
    for nodesB $ \node -> NodeRefRegistry.newNodeRef registry node

getValue :: (Transaction t, Monad (t db s), MonadIO (t db s)) => NodeRef db s -> t db s (Node.Value db)
getValue ref = liftIO $ NodeRef.getNode ref >>= Node.getValue

setValue :: Node.Value db -> NodeRef db s -> Write db s ()
setValue value ref = do
  liftIO $ do
    node <- NodeRef.getNode ref
    Node.setValue node value

insertEdge :: ( Hashable (Node.Edge db), Eq (Node.Edge db) ) =>
              Node.Edge db -> NodeRef db s -> NodeRef db s -> Write db s ()
insertEdge edge refA refB = do
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodeB <- NodeRef.getNode refB
    Node.insertEdge nodeA edge nodeB

deleteEdge :: Node.Edge db -> NodeRef db s -> NodeRef db s -> Write db s ()
deleteEdge edge refA refB = do
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodeB <- NodeRef.getNode refB
    Node.deleteEdge nodeA edge nodeB



