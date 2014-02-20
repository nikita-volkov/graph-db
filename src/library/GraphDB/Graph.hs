module GraphDB.Graph where

import GraphDB.Util.Prelude
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Union as U
import qualified GraphDB.Engine.Node as N
import qualified Control.Concurrent.FairRWLock as L

data Graph u = Graph !(U.Node u) !(L.RWLock)

instance (U.Union u) => B.Backend (Graph u) where
  type Tx (Graph u) = ReaderT (Graph u) IO
  newtype Node (Graph u) = Node (U.Node u)
  type Value (Graph u) = U.Value u
  type Type (Graph u) = U.Type u
  type Index (Graph u) = U.Index u
  runTx = runReaderT
  runRead tx g = let Graph _ l = g in L.withRead l $ B.runTx tx g
  runWrite tx g = let Graph _ l = g in L.withWrite l $ B.runTx tx g
  getRoot = do Graph root _ <- ask; return $ Node root
  getTargetsByType (Node n) t = liftIO $ N.getTargetsByType n t >>= return . map Node

