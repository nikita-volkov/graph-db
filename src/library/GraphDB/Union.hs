module GraphDB.Union where

import GraphDB.Util.Prelude hiding (Serializable)
import qualified GraphDB.Util.Prelude as P
import qualified GraphDB.Engine.Node as Node

class (Serializable IO u, Hashable (Type u), Eq (Type u), Hashable (Index u), Eq (Index u)) => Union u where
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

type Serializable m u = (P.Serializable m (Value u), P.Serializable m (Index u), P.Serializable m (Type u))


class (Union u) => PolyValue u v where
  packValue :: v -> (Type u, Value u)
  unpackValue :: Value u -> Maybe v

class (Union u) => PolyIndex u i where
  packIndex :: i -> Index u


