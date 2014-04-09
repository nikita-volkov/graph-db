module GraphDB.Model.Union where

import GraphDB.Util.Prelude hiding (Serializable)
import qualified GHC.Exts
import qualified GraphDB.Util.Prelude as P
import qualified GraphDB.Graph as Graph

-- |
-- An interface of a union data type, 
-- which is used as a unique label of a graph data model.
-- 
-- Its instances should be generated with 'GraphDB.Model.Macros.generateUnion'.
class (Serializable IO u, Hashable (Type u), Eq (Type u), Hashable (Index u), Eq (Index u)) => Union u where
  data Index u
  data Value u
  data Type u
  indexes :: Value u -> Type u -> [Index u]
  indexTargetType :: Index u -> Type u
  decomposeValue :: Value u -> (Type u, GHC.Exts.Any)
  composeValue :: Type u -> GHC.Exts.Any -> Value u

instance (Union u) => Graph.Type (Type u) where
  type Index (Type u) = Index u
  type Value (Type u) = Value u
  indexes = indexes
  decomposeValue = decomposeValue
  composeValue = composeValue
  targetType = indexTargetType

type Node u = Graph.Node (Type u)

type Serializable m u = (P.Serializable m (Value u), P.Serializable m (Index u), P.Serializable m (Type u))

-- |
-- An interface for conversion of a value to an internal representation.
-- 
-- Its instances should be generated with 'GraphDB.Model.Macros.generateUnion'.
class (Union u) => PolyValue u v where
  packValue :: v -> (Type u, Value u)
  unpackValue :: Value u -> Maybe v

-- |
-- An interface for conversion of an index to an internal representation.
-- 
-- Its instances should be generated with 'GraphDB.Model.Macros.generateUnion'.
class (Union u) => PolyIndex u i where
  packIndex :: i -> Index u


