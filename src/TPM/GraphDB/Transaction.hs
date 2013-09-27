module TPM.GraphDB.Transaction where

import TPM.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.DB as DB; import TPM.GraphDB.DB (DB)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)
import qualified TPM.GraphDB.Transaction.NodeRefRegistry as NodeRefRegistry; import TPM.GraphDB.Transaction.NodeRefRegistry (NodeRefRegistry)
import qualified TPM.GraphDB.Transaction.NodeRef as NodeRef; import TPM.GraphDB.Transaction.NodeRef (NodeRef)



class Transaction t where
  run :: DB tag -> (forall state. t tag state a) -> IO a
  getDB :: t tag state (DB tag)
  getNodeRefRegistry :: t tag state (NodeRefRegistry tag)



-- |
-- A write and read transaction. Only a single write-transaction executes at a time.
-- 
-- Here the /s/ is a state-thread making the escape of node-refs from transaction
-- impossible. Much inspired by the realization of 'ST'.
-- 
newtype Write tag s a = Write (DB tag -> NodeRefRegistry tag -> IO a)

instance Transaction Write where
  run db (Write dbToRegistryToIO) = Dispatcher.runWrite (DB.dispatcher db) io where 
    io = NodeRefRegistry.new >>= dbToRegistryToIO db
  getDB = Write $ \z _ -> return z
  getNodeRefRegistry = Write $ \_ z -> return z

instance MonadIO (Write tag s) where
  liftIO io = Write $ \_ _ -> io

instance Monad (Write tag s) where
  return a = Write $ \_ _ -> return a
  writeA >>= aToWriteB = Write dbToRegToIO where
    dbToRegToIO db reg = ioA >>= aToIOB where
      Write dbToRegToIOA = writeA
      ioA = dbToRegToIOA db reg
      aToIOB a = ioB where
        Write dbToRegToIOB = aToWriteB a
        ioB = dbToRegToIOB db reg

instance Applicative (Write tag s) where
  pure = return
  (<*>) = ap

instance Functor (Write tag s) where
  fmap f = (=<<) $ return . f



-- |
-- A read-only transaction. Gets executed concurrently.
-- 
-- Here the /s/ is a state-thread making the escape of node-refs from transaction
-- impossible. Much inspired by the realization of 'ST'.
-- 
newtype Read tag s a = Read (DB tag -> NodeRefRegistry tag -> IO a)

instance Transaction Read where
  run db (Read dbToRegistryToIO) = Dispatcher.runRead (DB.dispatcher db) io where 
    io = NodeRefRegistry.new >>= dbToRegistryToIO db
  getDB = Read $ \z _ -> return z
  getNodeRefRegistry = Read $ \_ z -> return z

instance MonadIO (Read tag s) where
  liftIO io = Read $ \_ _ -> io

instance Monad (Read tag s) where
  return a = Read $ \_ _ -> return a
  readA >>= aToReadB = Read dbToRegToIO where
    dbToRegToIO db reg = ioA >>= aToIOB where
      Read dbToRegToIOA = readA
      ioA = dbToRegToIOA db reg
      aToIOB a = ioB where
        Read dbToRegToIOB = aToReadB a
        ioB = dbToRegToIOB db reg

instance Applicative (Read tag s) where
  pure = return
  (<*>) = ap

instance Functor (Read tag s) where
  fmap f = (=<<) $ return . f



getRoot :: (Transaction t, Monad (t tag s), MonadIO (t tag s)) => t tag s (NodeRef tag s ())
getRoot = do
  root <- getDB >>= return . DB.root
  registry <- getNodeRefRegistry
  liftIO $ NodeRefRegistry.newNodeRef root registry

newNode :: a -> Write tag s (NodeRef tag s a)
newNode value = do
  registry <- getNodeRefRegistry
  liftIO $ do
    node <- Node.new value
    NodeRefRegistry.newNodeRef node registry

getTargets :: ( Transaction t, Monad (t tag s), MonadIO (t tag s) ) 
           => Node.Edge tag a b -> NodeRef tag s a -> t tag s [NodeRef tag s b]
getTargets edge refA = do
  registry <- getNodeRefRegistry
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodesB <- Node.getTargets edge nodeA
    for nodesB $ \node -> NodeRefRegistry.newNodeRef node registry

getValue :: (Transaction t, Monad (t tag s), MonadIO (t tag s)) => NodeRef tag s a -> t tag s a
getValue ref = liftIO $ NodeRef.getNode ref >>= Node.getValue

setValue :: a -> NodeRef tag s a -> Write tag s ()
setValue value ref = do
  liftIO $ do
    node <- NodeRef.getNode ref
    Node.setValue value node

insertEdge :: (Hashable (Node.Edge tag a b), Eq (Node.Edge tag a b), Typeable b) =>
              Node.Edge tag a b -> NodeRef tag s a -> NodeRef tag s b -> Write tag s ()
insertEdge edge refA refB = do
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodeB <- NodeRef.getNode refB
    Node.insertEdge edge nodeB nodeA

deleteEdge :: Node.Edge tag a b -> NodeRef tag s a -> NodeRef tag s b -> Write tag s ()
deleteEdge edge refA refB = do
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodeB <- NodeRef.getNode refB
    Node.deleteEdge edge nodeB nodeA



