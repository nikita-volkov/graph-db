{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Protocol where

import GraphDB.Util.Prelude
import qualified GraphDB.Graph as G


data Request s =
  Start Write |
  Finish |
  Action (Action s)
  deriving (Generic)

instance (Serializable m (G.Value s), Serializable m (G.Index s)) => Serializable m (Request s)

type Write = Bool

data Action s =
  NewNode (G.Value s) |
  GetValue Node |
  SetValue Node (G.Value s) |
  GetRoot |
  GetTargetsByIndex Node (G.Index s) |
  AddTarget Node Node |
  RemoveTarget Node Node |
  Remove Node |
  GetStats
  deriving (Generic)

instance (Serializable m (G.Value s), Serializable m (G.Index s)) => Serializable m (Action s)

type Node = Int

data Response s =
  Unit |
  Node Node |
  Value (G.Value s) |
  NodeList [Node] |
  Bool Bool |
  Stats G.Stats
  deriving (Generic)

instance (Serializable m (G.Value s), Serializable m (G.Index s)) => Serializable m (Response s)

