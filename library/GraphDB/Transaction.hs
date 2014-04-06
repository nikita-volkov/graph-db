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
import qualified GraphDB.Model.Edge as E


type Tx b u r = (B.Backend b, B.Union b ~ u) => B.Tx b r

-- |
-- A read-only transaction. Gets executed concurrently.
-- 
newtype Read b u s r = Read (Tx b u r)

instance Monad (Read b u s) where
  return a = Read $ return a
  Read a >>= k = Read $ a >>= return . k >>= \(Read b) -> b

instance Applicative (Read b u s) where 
  pure a = Read $ pure a
  Read a <*> Read b = Read $ a <*> b

instance Functor (Read b u s) where
  fmap f (Read a) = Read $ fmap f a


-- |
-- A write and read transaction.
-- 
newtype Write b u s r = Write (Tx b u r)

instance Monad (Write b u s) where
  return a = Write $ return a
  Write a >>= k = Write $ a >>= return . k >>= \(Write b) -> b

instance Applicative (Write b u s) where 
  pure a = Write $ pure a
  Write a <*> Write b = Write $ a <*> b

instance Functor (Write b u s) where
  fmap f (Write a) = Write $ fmap f a

-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite b u s r = forall t. (LiftTx t, Monad (t b u s), Applicative (t b u s)) => t b u s r

class LiftTx t where liftTx :: Tx b u r -> t b u s r
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
newNode :: (U.PolyValue u v) => v -> Write b u s (Node b s v)
newNode v = do
  bn <- liftTx $ B.newNode $ snd $ U.packValue v
  return $ Node bn

-- | 
-- Get a value of the node.
getValue :: forall b u s v. (U.PolyValue u v) => Node b s v -> ReadOrWrite b u s v
getValue (Node n) = do
  pv :: U.Value u <- liftTx $ B.getValue n
  return $ U.unpackValue pv ?: ($bug $ "Unexpected packed value")

-- | 
-- Replace the value of the specified node.
setValue :: (U.PolyValue u v) => Node b s v -> v -> Write b u s ()
setValue (Node n) v = Write $ B.setValue n (snd $ U.packValue v)

-- |
-- Get the root node.
getRoot :: ReadOrWrite b u s (Node b s u)
getRoot = liftTx $ B.getRoot >>= pure . Node

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType node (undefined :: Artist)
-- 
getTargetsByType :: (U.PolyValue u v') => Node b s v -> v' -> ReadOrWrite b u s [Node b s v']
getTargetsByType (Node n) v = do
  ns <- liftTx $ B.getTargetsByType n $ fst $ U.packValue v
  return $ map Node ns

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: 
  (U.PolyIndex u i) => 
  Node b s v -> i -> ReadOrWrite b u s [Node b s v']
getTargetsByIndex (Node n) i = do
  ns <- liftTx $ B.getTargetsByIndex n (U.packIndex i)
  return $ map Node ns

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is already there it will return 'False'.
addTarget :: (E.Edge v v') => Node b s v -> Node b s v' -> Write b u s Bool
addTarget (Node s) (Node t) = Write $ B.addTarget s t

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is not found it will return 'False'.
removeTarget :: (E.Edge v v') => Node b s v -> Node b s v' -> Write b u s Bool
removeTarget (Node s) (Node t) = Write $ B.removeTarget s t

-- |
-- Count the total amounts of distinct nodes and edges in the graph.
-- 
-- Requires a traversal of the whole graph, so beware.
getStats :: ReadOrWrite b u s (Int, Int)
getStats = do
  Node n <- getRoot
  liftTx $ B.getStats n

-- |
-- Run a 'Write' transaction on the specified backend.
runWrite :: B.Backend b => b -> (forall s. Write b (B.Union b) s r) -> IO r
runWrite b (Write tx) = B.runWrite tx b

-- |
-- Run a 'Read' transaction on the specified backend.
runRead :: B.Backend b => b -> (forall s. Read b (B.Union b) s r) -> IO r
runRead b (Read tx) = B.runRead tx b
