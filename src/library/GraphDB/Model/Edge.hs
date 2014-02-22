module GraphDB.Model.Edge where

import GraphDB.Util.Prelude


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

