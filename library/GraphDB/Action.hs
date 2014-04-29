-- |
-- Provides a low-level transaction over union types 
-- without distinction between write and read.
module GraphDB.Action where

import GraphDB.Util.Prelude


type Action n v i = FreeT (ActionF n v i)

data ActionF n v i a =
  NewNode v (n -> a) |
  GetValue n (v -> a) |
  SetValue n v a |
  GetRoot (n -> a) |
  GetTargets n i ([n] -> a) |
  AddTarget n n a |
  RemoveTarget n n a |
  Remove n a |
  GetStats ((Int, Int, Int) -> a)
  deriving (Functor)

makeFree ''ActionF

