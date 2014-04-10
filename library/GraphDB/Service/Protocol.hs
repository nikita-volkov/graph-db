{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Service.Protocol where

import GraphDB.Util.Prelude
import qualified GraphDB.Model.Union as U


data Request u =
  Start Write |
  Finish |
  Action (Action u)
  deriving (Generic)

instance (U.Serializable m u) => Serializable m (Request u)

type Write = Bool

data Action u =
  NewNode (U.Value u) |
  GetValue Node |
  SetValue Node (U.Value u) |
  GetRoot |
  GetTargetsByType Node (U.Type u) |
  GetTargetsByIndex Node (U.Index u) |
  AddTarget Node Node |
  RemoveTarget Node Node |
  GetStats
  deriving (Generic)

instance (U.Serializable m u) => Serializable m (Action u)

type Node = Int

data Response u =
  Unit |
  Node Node |
  Value (U.Value u) |
  NodeList [Node] |
  Bool Bool |
  IntPair (Int, Int)
  deriving (Generic)

instance (U.Serializable m u) => Serializable m (Response u)

