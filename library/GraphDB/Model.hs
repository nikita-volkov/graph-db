-- |
-- An API for declaration of a data model of a graph.
module GraphDB.Model
  (
    E.Edge(..),
    U.Union,
    U.PolyValue,
    U.PolyIndex,
    M.generateUnion,
  ) where

import GraphDB.Util.Prelude
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Model.Macros as M
import qualified GraphDB.Model.Edge as E

