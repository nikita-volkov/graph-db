-- |
-- A shared transactions composition and execution API
-- for all types implementing the 'B.Backend' interface.
-- 
-- The types in this module share some parameters, here is their description:
-- 
-- [@b@] A backend implementation. E.g., an in-memory or persisted graph or a client.
-- 
-- [@s@] A state-thread making the escape of nodes from transaction impossible. 
-- Much inspired by the realization of 'ST'.
module GraphDB.Transaction 
  (
    Write,
    Read,
    ReadOrWrite,
    Node,
    B.Backend,
    -- ** Execution
    runWrite,
    runRead,
    -- ** Operations
    newNode,
    getValue,
    setValue,
    getRoot,
    getTargetsByType,
    getTargetsByIndex,
    addTarget,
    removeTarget,
    getStats,
  )
  where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Model.Union as U


-- |
-- A read-only transaction. Gets executed concurrently.
-- 
newtype Read b u s r = Read (B.Tx b u r)

instance Monad (B.Tx b u) => Monad (Read b u s) where
  return = Read . return
  Read a >>= k = Read $ a >>= return . k >>= \(Read b) -> b

instance Applicative (B.Tx b u) => Applicative (Read b u s) where 
  pure = Read . pure
  Read a <*> Read b = Read $ a <*> b

instance Functor (B.Tx b u) => Functor (Read b u s) where
  fmap f (Read a) = Read $ fmap f a


-- |
-- A write and read transaction.
-- 
newtype Write b u s r = Write (B.Tx b u r)

instance Monad (B.Tx b u) => Monad (Write b u s) where
  return = Write . return
  Write a >>= k = Write $ a >>= return . k >>= \(Write b) -> b

instance Applicative (B.Tx b u) => Applicative (Write b u s) where 
  pure = Write . pure
  Write a <*> Write b = Write $ a <*> b

instance Functor (B.Tx b u) => Functor (Write b u s) where
  fmap f (Write a) = Write $ fmap f a

-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite b u s r = forall t. (LiftTx t, Monad (t b u s), Applicative (t b u s)) => t b u s r

class LiftTx t where liftTx :: B.Tx b u r -> t b u s r
instance LiftTx Read where liftTx = Read
instance LiftTx Write where liftTx = Write


-- | 
-- A transaction-local reference to the actual node of a graph-datastructure.
-- 
-- The /s/ is a state-thread making the escape of nodes from transaction
-- impossible. Much inspired by the realization of 'ST'.
newtype Node b u s v = Node (B.Node b u)


-- |
-- Create a new node. 
-- 
-- This node won't get stored if you don't insert at least a single edge 
-- from another stored node to it.
newNode :: (B.Backend b u, U.PolyValue u v) => v -> Write b u s (Node b u s v)
newNode v = do
  bn <- liftTx $ B.newNode $ snd $ U.packValue v
  return $ Node bn

-- | 
-- Get a value of the node.
getValue :: (B.Backend b u, U.PolyValue u v) => Node b u s v -> ReadOrWrite b u s v
getValue (Node n) = do
  pv <- liftTx $ B.getValue n
  return $ U.unpackValue pv ?: $(bug "Unexpected packed value")

-- | 
-- Replace the value of the specified node.
setValue :: (B.Backend b u, U.PolyValue u v) => Node b u s v -> v -> Write b u s ()
setValue (Node n) v = Write $ B.setValue n (snd $ U.packValue v)

-- |
-- Get the root node.
getRoot :: B.Backend b u => ReadOrWrite b u s (Node b u s v)
getRoot = liftTx $ B.getRoot >>= pure . Node

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType node (undefined :: Artist)
-- 
getTargetsByType :: 
  (B.Backend b u, U.PolyValue u v') => 
  Node b u s v -> v' -> ReadOrWrite b u s [Node b u s v']
getTargetsByType (Node n) v = do
  let (t, _) = U.packValue v
  ns <- liftTx $ B.getTargetsByType n t
  return $ map Node ns

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: 
  (B.Backend b u, U.PolyIndex u i) => 
  Node b u s v -> i -> ReadOrWrite b u s [Node b u s v']
getTargetsByIndex (Node n) i = do
  ns <- liftTx $ B.getTargetsByIndex n (U.packIndex i)
  return $ map Node ns

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is already there it will return 'False'.
addTarget :: B.Backend b u => Node b u s v -> Node b u s v' -> Write b u s Bool
addTarget (Node s) (Node t) = Write $ B.addTarget s t

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is not found it will return 'False'.
removeTarget :: B.Backend b u => Node b u s v -> Node b u s v' -> Write b u s Bool
removeTarget (Node s) (Node t) = Write $ B.removeTarget s t

-- |
-- Count the total amounts of distinct nodes and edges in the graph.
-- 
-- Requires a traversal of the whole graph, so beware.
getStats :: B.Backend b u => ReadOrWrite b u s (Int, Int)
getStats = do
  Node n <- getRoot
  liftTx $ B.getStats n

-- |
-- Run a 'Write' transaction on the specified backend.
runWrite :: B.Backend b u => b -> Write b u s r -> IO r
runWrite b (Write tx) = B.runWrite tx b

-- |
-- Run a 'Read' transaction on the specified backend.
runRead :: B.Backend b u => b -> Read b u s r -> IO r
runRead b (Read tx) = B.runRead tx b
