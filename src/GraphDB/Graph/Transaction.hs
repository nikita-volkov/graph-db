module GraphDB.Graph.Transaction where

import GraphDB.Prelude hiding (Read, Write, ReadOrWrite)
import GraphDB.Graph.Types
import qualified GraphDB.Graph.TypedNode as TypedNode; import GraphDB.Graph.TypedNode (TypedNode)



-- |
-- A transaction-local reference to node. 
-- 
-- The /s/ is a state-thread making the escape of nodes from transaction
-- impossible. Much inspired by the realization of 'ST'.
newtype Node t s v = Node (TypedNode t v)

-- |
-- Get the root node.
getRoot :: ReadOrWrite t s (Node t s (Root t))
getRoot = fmap Node getRootTypedNode

-- |
-- Create a new node. 
-- 
-- This node won't get stored if you don't insert at least a single edge 
-- from another stored node to it.
newNode :: v -> ReadOrWrite t s (Node t s v)
newNode = fmap Node . liftIO . TypedNode.new

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType (undefined :: Artist) ...
-- 
getTargetsByType :: (Reachable t v v', GraphTag t) => v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByType _ (Node source) = 
  liftIO $ map Node <$> TypedNode.getTargetsByType undefined source

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: (Reachable t v v', GraphTag t) => Index t v v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByIndex i (Node source) = 
  liftIO $ map Node <$> TypedNode.getTargetsByIndex i source

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
addTarget :: (Reachable t v v', GraphTag t) => Node t s v' -> Node t s v -> Write t s ()
addTarget (Node target) (Node source) = 
  liftIO $ TypedNode.addTarget target source

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
removeTarget :: (Reachable t v v', GraphTag t) => Node t s v' -> Node t s v -> Write t s ()
removeTarget (Node target) (Node source) = 
  liftIO $ TypedNode.removeTarget target source

-- | 
-- Get a value of the node.
getValue :: Node t s v -> ReadOrWrite t s v
getValue (Node n) = liftIO $ TypedNode.getValue n

-- | 
-- Replace a value of the specified node.
setValue :: v -> Node t s v -> Write t s ()
setValue a (Node n) = liftIO $ TypedNode.setValue a n

-- |
-- Count how many targets the node has.
countTargets :: Node t s v -> ReadOrWrite t s Int
countTargets (Node n) = liftIO $ TypedNode.countTargets n

-- |
-- Count the total amount of distinct nodes and edges in the graph.
-- 
-- Requires traversal of the whole graph, so beware.
getStats :: (GraphTag t) => ReadOrWrite t s (Int, Int)
getStats = do
  Node tn <- getRoot
  liftIO $ TypedNode.getStats tn



-- | Support for common operations of transaction.
class Transaction t where
  getRootTypedNode :: t tag s (TypedNode tag (Root tag))

instance Transaction Write where
  getRootTypedNode = Write $ \z -> return z

instance Transaction Read where
  getRootTypedNode = Read $ \z -> return z



-- |
-- A write and read transaction.
-- 
newtype Write t s z = Write (TypedNode t (Root t) -> IO z)

instance MonadIO (Write t s) where
  liftIO io = Write $ \_ -> io

instance Monad (Write t s) where
  return a = Write $ \_ -> return a
  writeA >>= aToWriteB = Write rootToIO where
    rootToIO root = ioA >>= aToIOB where
      Write rootToIOA = writeA
      ioA = rootToIOA root
      aToIOB a = ioB where
        Write rootToIOB = aToWriteB a
        ioB = rootToIOB root

instance Applicative (Write t s) where
  pure = return
  (<*>) = ap

instance Functor (Write t s) where
  fmap = liftM

runWrite :: TypedNode t (Root t) -> (forall s. Write t s z) -> IO z
runWrite root (Write run) = run root



-- |
-- A read-only transaction. Gets executed concurrently.
-- 
newtype Read t s z = Read (TypedNode t (Root t) -> IO z)

instance MonadIO (Read t s) where
  liftIO io = Read $ \_ -> io

instance Monad (Read t s) where
  return a = Read $ \_ -> return a
  readA >>= aToReadB = Read rootToIO where
    rootToIO root = ioA >>= aToIOB where
      Read rootToIOA = readA
      ioA = rootToIOA root
      aToIOB a = ioB where
        Read rootToIOB = aToReadB a
        ioB = rootToIOB root

instance Applicative (Read t s) where
  pure = return
  (<*>) = ap

instance Functor (Read t s) where
  fmap = liftM

runRead :: TypedNode t (Root t) -> (forall s. Read t s z) -> IO z
runRead root (Read run) = run root


-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite t s z = 
  forall tr. (Transaction tr, MonadIO (tr t s), Applicative (tr t s)) => tr t s z
