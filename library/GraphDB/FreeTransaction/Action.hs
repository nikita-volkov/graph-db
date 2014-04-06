-- |
-- Provides a low-level transaction over union types 
-- without distinction between write and read.
module GraphDB.FreeTransaction.Action where

import GraphDB.Util.Prelude hiding (Read, Write, read, write)
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Model.Edge as E


type Action b u = Free (ActionF b u)

data ActionF b u a =
  NewNode (U.Value u) (Node b -> a) |
  GetValue (Node b) ((U.Value u) -> a) |
  SetValue (Node b) (U.Value u) a |
  GetRoot (Node b -> a) |
  GetTargetsByType (Node b) (U.Type u) ([Node b] -> a) |
  GetTargetsByIndex (Node b) (U.Index u) ([Node b] -> a) |
  AddTarget (Node b) (Node b) (Bool -> a) |
  RemoveTarget (Node b) (Node b) (Bool -> a) |
  GetStats (Node b) ((Int, Int) -> a)
  deriving (Functor)

type family Node b

makeFree ''ActionF
