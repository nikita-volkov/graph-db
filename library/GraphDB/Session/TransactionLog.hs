{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Session.TransactionLog where

import GraphDB.Util.Prelude
import qualified GraphDB.Model.Union as U
import qualified GraphDB.FreeTransaction.Action as A

-- |
-- A serializable reproduction of all the modifications to the graph done during a transaction.
newtype Log u = Log [Entry u] deriving (Generic)
instance (Serializable m (Entry u)) => Serializable m (Log u)

-- |
-- A serializable representation of a granular transaction action.
data Entry u =
  GetRoot |
  NewNode (U.Value u) |
  GetTargetsByType Ref (U.Type u) |
  GetTargetsByIndex Ref (U.Index u) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  SetValue Ref (U.Value u)
  deriving (Generic)
instance (U.Serializable m u) => Serializable m (Entry u)

type Ref = Int
