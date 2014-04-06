-- |
-- This API does two things:
-- 1. Provides a distinction between Write and Read transactions,
-- 2. Wraps the conversion between normal and union values.
module GraphDB.FreeTransaction where

import GraphDB.Util.Prelude hiding (Read, Write, read, write)
import Control.Monad.Free
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Model.Edge as E
import qualified GraphDB.FreeTransaction.Action as A


newtype Write b u s r = Write (A.Action b u r) deriving (Functor, Applicative, Monad)
newtype Read b u s r = Read (A.Action b u r) deriving (Functor, Applicative, Monad)
-- |
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite b u s r = forall t. (LiftAction t, Monad (t b u s), Applicative (t b u s)) => t b u s r

class LiftAction t where liftAction :: A.Action b u r -> t b u s r
instance LiftAction Read where liftAction = Read
instance LiftAction Write where liftAction = Write

newtype Node b s v = Node (A.Node b)

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
getRoot = liftAction $ A.getRoot >>= pure . Node

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType node (undefined :: Artist)
-- 
getTargetsByType :: (U.PolyValue u v') => Node b s v -> v' -> ReadOrWrite b u s [Node b s v']
getTargetsByType (Node n) v = do
  ns <- liftAction $ A.getTargetsByType n $ fst $ U.packValue v
  return $ map Node ns

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: 
  (U.PolyIndex u i) => 
  Node b s v -> i -> ReadOrWrite b u s [Node b s v']
getTargetsByIndex (Node n) i = do
  ns <- liftAction $ A.getTargetsByIndex n (U.packIndex i)
  return $ map Node ns

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
getStats = do
  Node n <- getRoot
  liftAction $ A.getStats n

