module GraphDB.Union where

import GraphDB.Util.Prelude
import qualified GraphDB.Engine.Node as Node

class (SerializableUnion u, Hashable (Type u), Eq (Type u), Hashable (Index u), Eq (Index u)) => Union u where
  data Index u
  data Value u
  data Type u
  indexes :: Value u -> Type u -> [Index u]
  indexTargetType :: Index u -> Type u
  decomposeValue :: Value u -> (Type u, Any)
  composeValue :: Type u -> Any -> Value u

instance (Union u) => Node.Type (Type u) where
  type Index (Type u) = Index u
  type Value (Type u) = Value u
  indexes = indexes
  decomposeValue = decomposeValue
  composeValue = composeValue
  targetType = indexTargetType

type Node u = Node.Node (Type u)

type SerializableUnion u = (Serializable IO (Value u), Serializable IO (Index u), Serializable IO (Type u))

