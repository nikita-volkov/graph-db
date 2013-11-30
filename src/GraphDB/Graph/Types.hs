module GraphDB.Graph.Types where

import GraphDB.Prelude


-- |
-- Determines the settings for a graph, tagged with this type.
-- 
-- It's normal to use the type of a root-node for this.
class 
  (IsUnionValue t (Root t), 
   Serializable IO (UnionValue t), 
   Serializable IO (UnionValueType t),
   Hashable (UnionValueType t),
   Eq (UnionValueType t)) => 
  GraphTag t 
  where
    -- | A type of a root-node.
    type Root t
    -- | A more memory-efficient alternative to 'TypeRep', used for grouping by type.
    data UnionValueType t
    data UnionValue t
    unionIndexHashes :: (UnionValue t, UnionValueType t) -> [Int]

class (Serializable IO v) => IsUnionValue t v where
  toUnionValueType :: v -> UnionValueType t
  toUnionValue :: v -> UnionValue t
  fromUnionValue :: UnionValue t -> Maybe v

-- |
-- Defines a specific set of indexes on nodes of value /v'/ for nodes of value /v/.
-- 
-- E.g., an artist may be referred from a root by its UID and search terms,
-- however, for an album it may emit no indexes at all, and so may only
-- be reached as an element of a list of all linked artists.
-- 
-- If there is no instance of this class between two values, then they can have
-- no links at all.
-- 
class 
  (IsUnionValue t v, IsUnionValue t v', Hashable (Index t v v'), Eq (Index t v v')) => 
  Reachable t v v'
  where
    data Index t v v'
    indexes :: v' -> [Index t v v']
    indexes = const []

