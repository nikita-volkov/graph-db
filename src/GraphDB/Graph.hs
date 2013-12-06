module GraphDB.Graph
  (
    Graph,
    new,
    UnionNode.NodeValue(..),
    runWrite,
    runRead,
    Read,
    Write,
    getRoot,
    newNode,
    getTargetsByType,
    getTargetsByIndex,
    addTarget,
    removeTarget,
    getValue,
    setValue,
    getStats,
  )
  where

import GraphDB.Prelude hiding (Read, Write)
import qualified GraphDB.Graph.Node as UnionNode
import qualified GraphDB.Graph.Dispatcher as Dispatcher; import GraphDB.Graph.Dispatcher (Dispatcher)


-- | A mutable graph data structure over nodes of supported types.
data Graph t = Graph {
  root :: UnionNode t,
  dispatcher :: Dispatcher
}

-- | Initialize a 'Graph' with a value for a root-node.
new :: (ToUnionValue t (Root t)) => Root t -> IO (Graph t)
new value = Graph <$> UnionNode.new (toUnionValue value) <*> Dispatcher.new

-- |
-- Run a write-transaction. 
-- /s/ is a state-thread making the escape of 'NodeRef's from transaction impossible.
runWrite :: Graph t -> (forall s. Write t s z) -> IO z
runWrite graph (Write run) = Dispatcher.runWrite (dispatcher graph) $ run (root graph)

-- |
-- Run a read-transaction. 
-- /s/ is a state-thread making the escape of 'NodeRef's from transaction impossible.
runRead :: Graph t -> (forall s. Read t s z) -> IO z
runRead graph (Read run) = Dispatcher.runRead (dispatcher graph) $ run (root graph)


instance (UnionNode.NodeValue (UnionValue t)) => Serializable IO (Graph t) where
  serialize (Graph root _) = serialize root
  deserialize = Graph <$> deserialize <*> liftIO Dispatcher.new


-- | A type of a root-node.
type family Root t
data family UnionValue t

type UnionNode t = UnionNode.Node (UnionValue t)
type UnionValueType t = UnionNode.NodeValueType (UnionValue t)
type UnionIndex t = UnionNode.NodeValueIndex (UnionValue t)


-- |
-- Defines a specific set of indexes on nodes of value /v'/ for nodes of value /v/.
-- 
-- E.g., an artist may be referred from a root by its UID and search terms,
-- however, for an album it may emit no indexes at all, and so may only
-- be reached as an element of a list of all linked artists.
-- 
-- If there is no instance of this class between two values, 
-- then the associated nodes cannot be linked.
-- 
class 
  (UnionNode.NodeValue (UnionValue t), ToUnionValue t v, ToUnionValue t v', ToUnionIndex t (Index t v v')) => 
  Reachable t v v'
  where
    data Index t v v'
    indexes :: v' -> [Index t v v']
    indexes = const []

class (UnionNode.NodeValue (UnionValue t)) => ToUnionValue t v where
  toUnionValue :: v -> UnionValue t

class (UnionNode.NodeValue (UnionValue t)) => ToUnionIndex t v where
  toUnionIndex :: v -> UnionIndex t

-- TODO: eliminate it
toUnionValueType :: (ToUnionValue t v, UnionNode.NodeValue (UnionValue t)) => v -> UnionValueType t
toUnionValueType = UnionNode.nodeValueType . toUnionValue


-- |
-- A transaction-local reference to node. 
-- 
-- The /s/ is a state-thread making the escape of nodes from transaction
-- impossible. Much inspired by the realization of 'ST'.
newtype Node t s v = Node (UnionNode t) deriving (Eq)

-- |
-- Get the root node.
getRoot :: ReadOrWrite t s (Node t s (Root t))
getRoot = fmap Node getRootUnionNode

-- |
-- Create a new node. 
-- 
-- This node won't get stored if you don't insert at least a single edge 
-- from another stored node to it.
newNode :: (ToUnionValue t v) => v -> ReadOrWrite t s (Node t s v)
newNode = fmap Node . liftIO . UnionNode.new . toUnionValue

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType (undefined :: Artist) ...
-- 
getTargetsByType :: (Reachable t v v') => v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByType v (Node source) =
  liftIO $ map Node <$> UnionNode.getTargetsByType source (toUnionValueType v)

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: (Reachable t v v') => Index t v v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByIndex i (Node source) = 
  liftIO $ map Node <$> UnionNode.getTargetsByIndex source (toUnionIndex i)

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node was already there it would not be.
addTarget :: (Reachable t v v') => Node t s v' -> Node t s v -> Write t s Bool
addTarget (Node target) (Node source) = 
  liftIO $ UnionNode.addTarget target source

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node was already there it would not be.
removeTarget :: (Reachable t v v') => Node t s v' -> Node t s v -> Write t s Bool
removeTarget (Node target) (Node source) = 
  liftIO $ UnionNode.removeTarget target source

-- | 
-- Get the value of the node.
getValue :: (UnionNode.NodeValue (UnionValue t)) => Node t s v -> ReadOrWrite t s v
getValue (Node n) = liftIO $ unsafeCoerce . UnionNode.nodeValueAny <$> UnionNode.getValue n

-- | 
-- Replace the value of the specified node.
setValue :: (ToUnionValue t v) => v -> Node t s v -> Write t s ()
setValue a (Node n) = liftIO $ UnionNode.setValue n (toUnionValue a)

-- |
-- Count the total amount of distinct nodes and edges in the graph.
-- 
-- Requires traversal of the whole graph, so beware.
getStats :: (UnionNode.NodeValue (UnionValue t)) => ReadOrWrite t s (Int, Int)
getStats = do
  Node tn <- getRoot
  liftIO $ UnionNode.getStats tn



-- | Support for common operations of a transaction.
class Transaction t where
  getRootUnionNode :: t tag s (UnionNode tag)

instance Transaction Write where
  getRootUnionNode = Write $ \z -> return z

instance Transaction Read where
  getRootUnionNode = Read $ \z -> return z



-- |
-- A write and read transaction.
-- 
newtype Write t s z = Write (UnionNode t -> IO z)

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


-- |
-- A read-only transaction. Gets executed concurrently.
-- 
newtype Read t s z = Read (UnionNode t -> IO z)

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


-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite t s z = 
  forall tr. (Transaction tr, MonadIO (tr t s), Applicative (tr t s)) => tr t s z
