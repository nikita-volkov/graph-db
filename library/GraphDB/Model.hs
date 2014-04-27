module GraphDB.Model where

import GraphDB.Util.Prelude hiding (Serializable)
import qualified GHC.Exts
import qualified GraphDB.Util.Prelude as P
import qualified GraphDB.Graph as G


type Serializable m s = (P.Serializable m (G.Value s), P.Serializable m (G.Index s))

-- |
-- An interface for conversion of a value to an internal representation.
-- 
-- Its instances should be generated with 'GraphDB.Model.Macros.deriveUnion'.
class (G.Setup s) => PolyValue s v where
  packValue :: v -> G.Value s
  unpackValue :: G.Value s -> Maybe v

-- |
-- An interface for conversion of an index to an internal representation.
-- 
-- Its instances should be generated with 'GraphDB.Model.Macros.deriveUnion'.
class (G.Setup s) => PolyIndex s i where
  packIndex :: i -> G.Index s


-- |
-- Defines a specific set of indexes, which nodes of value /v'/ emit to nodes of value /v/.
-- 
-- If the indexes list is empty, 
-- the node may still be reached thru 'getTargetsByType'.
-- 
-- If there is no instance of this class between two values, 
-- then the associated nodes cannot be linked.
-- 
class Edge v v' where
  data Index v v'
  indexes :: v' -> [Index v v']
  indexes = const []


