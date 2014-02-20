module GraphDB.Graph where

import GraphDB.Util.Prelude
import qualified GraphDB.Transaction.Tx as T
import qualified GraphDB.Union as U
import qualified GraphDB.Engine.Node as N

newtype Graph u = Graph (U.Node u)

instance (U.Union u) => T.Backend (Graph u) where
  type Node (Graph u) = U.Node u
  type Value (Graph u) = U.Value u
  type Type (Graph u) = U.Type u
  type Index (Graph u) = U.Index u
  getRoot = do Graph root <- ask; return root
  getTargetsByType n t = liftIO $ N.getTargetsByType n t

