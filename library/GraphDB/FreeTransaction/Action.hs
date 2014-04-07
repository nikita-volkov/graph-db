-- |
-- Provides a low-level transaction over union types 
-- without distinction between write and read.
module GraphDB.FreeTransaction.Action where

import GraphDB.Util.Prelude hiding (Read, Write, read, write)
import qualified GraphDB.Model.Union as U


type Action n u = Free (ActionF n u)

-- TODO: Lose the Union dependency by parameterizing the type
data ActionF n u a =
  NewNode (U.Value u) (n -> a) |
  GetValue n ((U.Value u) -> a) |
  SetValue n (U.Value u) a |
  GetRoot (n -> a) |
  GetTargetsByType n (U.Type u) ([n] -> a) |
  GetTargetsByIndex n (U.Index u) ([n] -> a) |
  AddTarget n n (Bool -> a) |
  RemoveTarget n n (Bool -> a) |
  GetStats ((Int, Int) -> a)
  deriving (Functor)

makeFree ''ActionF

