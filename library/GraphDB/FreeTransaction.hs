-- |
-- This API does two things:
-- 1. Provides a distinction between Write and Read transactions,
-- 2. Wraps the conversion between normal and union values.
module GraphDB.FreeTransaction where

import GraphDB.Util.Prelude hiding (Read, Write, read, write)
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Model.Edge as E
import qualified GraphDB.FreeTransaction.Action as A


-- | 
-- A write and read transaction.
-- 
-- Does not allow concurrency, 
-- so all concurrent transactions are put on hold for the time of its execution.
newtype Write backend union stateThread result = 
  Write (A.Action backend union result) 
  deriving (Functor, Applicative, Monad)

-- | 
-- A read-only transaction. 
-- 
-- Gets executed concurrently.
newtype Read backend union stateThread result = 
  Read (A.Action backend union result) 
  deriving (Functor, Applicative, Monad)

-- |
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite b u s r = 
  forall t. (LiftAction t, Monad (t b u s), Applicative (t b u s)) => 
  t b u s r

class LiftAction t where liftAction :: A.Action b u r -> t b u s r
instance LiftAction Read where liftAction = Read
instance LiftAction Write where liftAction = Write

-- | 
-- A transaction-local reference to the actual node of a graph-datastructure.
-- 
-- The /stateThread/ is an uninstantiated type-variable,
-- which makes it impossible to return a node from transaction,
-- when it is executed using 'write' or 'read'.
-- Much inspired by the implementation of 'ST'.
newtype Node backend stateThread value = Node (A.Node backend)

-- |
-- Create a new node. 
-- 
-- This node won't get stored if you don't insert at least a single edge 
-- from another stored node to it.
newNode :: (U.PolyValue u v) => v -> Write b u s (Node b s v)
newNode v = fmap Node $ liftAction $ A.newNode $ snd $ U.packValue v

-- | 
-- Get a value of the node.
getValue :: (U.PolyValue u v) => Node b s v -> ReadOrWrite b u s v
getValue (Node n) = 
  fmap (fromMaybe ($bug "Unexpected packed value") . U.unpackValue) $ 
  liftAction $ A.getValue n

-- | 
-- Replace the value of the specified node.
setValue :: (U.PolyValue u v) => Node b s v -> v -> Write b u s ()
setValue (Node n) v = Write $ A.setValue n (snd $ U.packValue v)

-- |
-- Get the root node.
getRoot :: ReadOrWrite b u s (Node b s u)
getRoot = fmap Node $ liftAction $ A.getRoot

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType node (undefined :: Artist)
-- 
getTargetsByType :: (U.PolyValue u v') => Node b s v -> v' -> ReadOrWrite b u s [Node b s v']
getTargetsByType (Node n) v =
  fmap (map Node) $ liftAction $ A.getTargetsByType n $ fst $ U.packValue v

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: (U.PolyIndex u i) => Node b s v -> i -> ReadOrWrite b u s [Node b s v']
getTargetsByIndex (Node n) i = 
  fmap (map Node) $ liftAction $ A.getTargetsByIndex n $ U.packIndex i

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is already there it will return 'False'.
addTarget :: (E.Edge v v') => Node b s v -> Node b s v' -> Write b u s Bool
addTarget (Node s) (Node t) = Write $ A.addTarget s t

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is not found it will return 'False'.
removeTarget :: (E.Edge v v') => Node b s v -> Node b s v' -> Write b u s Bool
removeTarget (Node s) (Node t) = Write $ A.removeTarget s t

-- |
-- Count the total amounts of distinct nodes and edges in the graph.
-- 
-- Requires a traversal of the whole graph, so beware.
getStats :: ReadOrWrite b u s (Int, Int)
getStats = liftAction $ A.getStats

