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


-- |
-- A read-only transaction. Gets executed concurrently.
-- 
newtype Read b s r = Read (B.Tx b r)

instance Monad (B.Tx b) => Monad (Read b s) where
  return = Read . return
  Read a >>= k = Read $ a >>= return . k >>= \(Read b) -> b

instance Applicative (B.Tx b) => Applicative (Read b s) where 
  pure = Read . pure
  Read a <*> Read b = Read $ a <*> b

instance Functor (B.Tx b) => Functor (Read b s) where
  fmap f (Read a) = Read $ fmap f a


-- |
-- A write and read transaction.
-- 
newtype Write b s r = Write (B.Tx b r)

instance Monad (B.Tx b) => Monad (Write b s) where
  return = Write . return
  Write a >>= k = Write $ a >>= return . k >>= \(Write b) -> b

instance Applicative (B.Tx b) => Applicative (Write b s) where 
  pure = Write . pure
  Write a <*> Write b = Write $ a <*> b

instance Functor (B.Tx b) => Functor (Write b s) where
  fmap f (Write a) = Write $ fmap f a

-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite b s r = forall t. (LiftTx t, MonadIO (t b s), Applicative (t b s)) => t b s r

class LiftTx t where liftTx :: B.Tx b r -> t b s r
instance LiftTx Read where liftTx = Read
instance LiftTx Write where liftTx = Write


-- | 
-- A transaction-local reference to the actual node of a graph-datastructure.
-- 
-- The /s/ is a state-thread making the escape of nodes from transaction
-- impossible. Much inspired by the realization of 'ST'.
newtype Node b s v = Node (B.Node b)


-- |
-- Create a new node. 
-- 
-- This node won't get stored if you don't insert at least a single edge 
-- from another stored node to it.
newNode :: (B.Backend b, B.PolyValue b v) => v -> Write b s (Node b s v)
newNode v = do
  bn <- liftTx $ B.newNode $ snd $ B.packValue v
  return $ Node bn

-- | 
-- Get a value of the node.
getValue :: (B.Backend b, B.PolyValue b v) => Node b s v -> ReadOrWrite b s v
getValue (Node n) = do
  pv <- liftTx $ B.getValue n
  return $ B.unpackValue pv ?: $(bug "Unexpected packed value")

-- | 
-- Replace the value of the specified node.
setValue :: (B.Backend b, B.PolyValue b v) => Node b s v -> v -> Write b s ()
setValue (Node n) v = Write $ B.setValue n (snd $ B.packValue v)

-- |
-- Get the root node.
getRoot :: B.Backend b => ReadOrWrite b s (Node b s v)
getRoot = liftTx $ B.getRoot >>= pure . Node

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType node (undefined :: Artist)
-- 
getTargetsByType :: 
  (B.Backend b, B.PolyValue b v') => 
  Node b s v -> v' -> ReadOrWrite b s [Node b s v']
getTargetsByType (Node n) v = do
  let (t, _) = B.packValue v
  ns <- liftTx $ B.getTargetsByType n t
  return $ map Node ns

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: 
  (B.Backend b, B.PolyIndex b i) => 
  Node b s v -> i -> ReadOrWrite b s [Node b s v']
getTargetsByIndex (Node n) i = do
  ns <- liftTx $ B.getTargetsByIndex n (B.packIndex i)
  return $ map Node ns

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is already there it will return 'False'.
addTarget :: B.Backend b => Node b s v -> Node b s v' -> Write b s Bool
addTarget (Node s) (Node t) = Write $ B.addTarget s t

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is not found it will return 'False'.
removeTarget :: B.Backend b => Node b s v -> Node b s v' -> Write b s Bool
removeTarget (Node s) (Node t) = Write $ B.removeTarget s t

-- |
-- Count the total amounts of distinct nodes and edges in the graph.
-- 
-- Requires a traversal of the whole graph, so beware.
getStats :: B.Backend b => ReadOrWrite b s (Int, Int)
getStats = do
  Node n <- getRoot
  liftTx $ B.getStats n

-- |
-- Run a 'Write' transaction on the specified backend.
runWrite :: B.Backend b => Write b s r -> b -> IO r
runWrite (Write tx) = B.runWrite tx

-- |
-- Run a 'Read' transaction on the specified backend.
runRead :: B.Backend b => Read b s r -> b -> IO r
runRead (Read tx) = B.runRead tx
