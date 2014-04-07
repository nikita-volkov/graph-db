-- |
-- Provides a low-level transaction over union types 
-- without distinction between write and read.
module GraphDB.FreeTransaction.Action where

import GraphDB.Util.Prelude hiding (Read, Write, read, write)
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Model.Edge as E


type Action t u = Free (ActionF t u)

data ActionF t u a =
  NewNode (U.Value u) (Node t -> a) |
  GetValue (Node t) ((U.Value u) -> a) |
  SetValue (Node t) (U.Value u) a |
  GetRoot (Node t -> a) |
  GetTargetsByType (Node t) (U.Type u) ([Node t] -> a) |
  GetTargetsByIndex (Node t) (U.Index u) ([Node t] -> a) |
  AddTarget (Node t) (Node t) (Bool -> a) |
  RemoveTarget (Node t) (Node t) (Bool -> a) |
  GetStats ((Int, Int) -> a)
  deriving (Functor)

class Tx t where
  type Node t
  runAction :: (Monad (t u)) => Action t u r -> t u r
  
makeFree ''ActionF

