module GraphDB.Graph.Transaction where

import GraphDB.Prelude hiding (Read, Write, Any)
import qualified GraphDB.Graph.Node as Node; import GraphDB.Graph.Node (Node)
import qualified GraphDB.Graph.Transaction.NodeRefRegistry as NodeRefRegistry; import GraphDB.Graph.Transaction.NodeRefRegistry (NodeRefRegistry)
import qualified GraphDB.Graph.Transaction.NodeRef as NodeRef; import GraphDB.Graph.Transaction.NodeRef (NodeRef)



getRoot :: Any n e s (NodeRef n e s)
getRoot = do
  root <- getRootNode
  registry <- getNodeRefRegistry
  liftIO $ NodeRefRegistry.newNodeRef registry root

newNode :: n -> Any n e s (NodeRef n e s)
newNode value = do
  registry <- getNodeRefRegistry
  liftIO $ do
    node <- Node.new value
    NodeRefRegistry.newNodeRef registry node

getTargets :: (Hashable e, Eq e) => e -> NodeRef n e s -> Any n e s [NodeRef n e s]
getTargets edge refA = do
  registry <- getNodeRefRegistry
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodesB <- Node.getTargets nodeA edge
    forM nodesB $ \node -> NodeRefRegistry.newNodeRef registry node

getValue :: NodeRef n e s -> Any n e s n
getValue ref = liftIO $ NodeRef.getNode ref >>= Node.getValue

setValue :: NodeRef n e s -> n -> Write n e s ()
setValue ref value = do
  liftIO $ do
    node <- NodeRef.getNode ref
    Node.setValue node value

insertEdgeTo :: (Hashable e, Eq e) => NodeRef n e s -> e -> NodeRef n e s -> Write n e s ()
insertEdgeTo refA edge refB = do
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodeB <- NodeRef.getNode refB
    Node.insertEdgeTo nodeA edge nodeB

deleteEdgeTo :: (Hashable e, Eq e) => NodeRef n e s -> e -> NodeRef n e s -> Write n e s ()
deleteEdgeTo refA edge refB = do
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    nodeB <- NodeRef.getNode refB
    Node.deleteEdgeTo nodeA edge nodeB

deleteEdge :: (Hashable e, Eq e) => NodeRef n e s -> e -> Write n e s ()
deleteEdge refA edge = do
  liftIO $ do
    nodeA <- NodeRef.getNode refA
    Node.deleteEdge nodeA edge



-- | Support for common operations of transaction.
class Transaction t where
  getRootNode :: t n e s (Node n e)
  getNodeRefRegistry :: t n e s (NodeRefRegistry n e)

instance Transaction Write where
  getRootNode = Write $ \z _ -> return z
  getNodeRefRegistry = Write $ \_ z -> return z

instance Transaction Read where
  getRootNode = Read $ \z _ -> return z
  getNodeRefRegistry = Read $ \_ z -> return z



-- |
-- A write and read transaction. Only a single write-transaction executes at a time.
-- 
-- Here the /s/ is a state-thread making the escape of node-refs from transaction
-- impossible. Much inspired by the realization of 'ST'.
-- 
newtype Write n e s r = Write (Node n e -> NodeRefRegistry n e -> IO r)

instance MonadIO (Write n e s) where
  liftIO io = Write $ \_ _ -> io

instance Monad (Write n e s) where
  return a = Write $ \_ _ -> return a
  writeA >>= aToWriteB = Write rootToRegToIO where
    rootToRegToIO tag reg = ioA >>= aToIOB where
      Write rootToRegToIOA = writeA
      ioA = rootToRegToIOA tag reg
      aToIOB a = ioB where
        Write rootToRegToIOB = aToWriteB a
        ioB = rootToRegToIOB tag reg

instance Applicative (Write n e s) where
  pure = return
  (<*>) = ap

instance Functor (Write n e s) where
  fmap = liftM

runWrite :: Node n e -> (forall s. Write n e s r) -> IO r
runWrite root (Write run) = NodeRefRegistry.new >>= run root



-- |
-- A read-only transaction. Gets executed concurrently.
-- 
-- Here the /s/ is a state-thread making the escape of node-refs from transaction
-- impossible. Much inspired by the realization of 'ST'.
-- 
newtype Read n e s r = Read (Node n e -> NodeRefRegistry n e -> IO r)

instance MonadIO (Read n e s) where
  liftIO io = Read $ \_ _ -> io

instance Monad (Read n e s) where
  return a = Read $ \_ _ -> return a
  readA >>= aToReadB = Read rootToRegToIO where
    rootToRegToIO tag reg = ioA >>= aToIOB where
      Read rootToRegToIOA = readA
      ioA = rootToRegToIOA tag reg
      aToIOB a = ioB where
        Read rootToRegToIOB = aToReadB a
        ioB = rootToRegToIOB tag reg

instance Applicative (Read n e s) where
  pure = return
  (<*>) = ap

instance Functor (Read n e s) where
  fmap = liftM

runRead :: Node n e -> (forall s. Read n e s r) -> IO r
runRead root (Read run) = NodeRefRegistry.new >>= run root



type Any n e s r = forall t. (Transaction t, MonadIO (t n e s), Applicative (t n e s)) => t n e s r
