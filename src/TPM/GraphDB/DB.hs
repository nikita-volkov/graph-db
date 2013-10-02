module TPM.GraphDB.DB where

import TPM.GraphDB.Prelude
import qualified Data.Map as Map
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



-- | The Graph Database.
data DB db = DB {
  root :: Node db,
  dispatcher :: Dispatcher
}

-- | Initialize a 'DB' with only a single node having a /unit/-value.
new :: (IsUnionValueOf () db) => IO (DB db)
new = DB <$> Node.new (toUnionValue ()) <*> Dispatcher.new

-- | 
-- A union type for all node values to be used with /db/, required for serialization purposes. E.g.:
-- 
-- @
-- data instance DB.UnionValue Catalogue = UnitValue () | ArtistValue Artist | GenreValue Genre
-- @
-- 
data family UnionValue db
type instance Node.Value db = UnionValue db

-- | 
-- A union type for all edges to be used with /db/, required for serialization purposes. E.g.:
-- 
-- @
-- data instance DB.UnionEdge Catalogue = 
--   UnitToArtistEdge (DB.Edge () Artist) |
--   UnitToGenreEdge (DB.Edge () Genre) |
--   ArtistToGenreEdge (DB.Edge Artist Genre)
-- @
data family UnionEdge db
type instance Node.Edge db = UnionEdge db

-- |
-- Functions for converting a value to and from a union value.
class IsUnionValueOf v db where
  toUnionValue :: v -> UnionValue db
  fromUnionValue :: UnionValue db -> Maybe v

-- |
-- Functions for converting an edge to and from a union value.
class (Hashable (UnionEdge db), Eq (UnionEdge db)) => IsUnionEdgeOf e db where
  toUnionEdge :: e -> UnionEdge db
  fromUnionEdge :: UnionEdge db -> Maybe e

-- |
-- Properties of edge from /source/ to /target/. E.g.:
-- 
-- @
-- data instance DB.Edge () Artist = UnitToArtistByNameEdge Text | UnitToArtistEdge deriving (Eq, Show, Generic)
-- data instance DB.Edge Artist Genre = ArtistToGenreEdge deriving (Eq, Show, Generic)
-- data instance DB.Edge () Genre = UnitToGenreByNameEdge Text deriving (Eq, Show, Generic)
-- @
-- 
data family Edge source target
