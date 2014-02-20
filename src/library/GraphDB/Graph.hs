module GraphDB.Graph
  ( 
    Graph,
    new,
  )
  where

import GraphDB.Util.Prelude
import qualified GraphDB.Transaction
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Union as U
import qualified GraphDB.Engine.Node as N
import qualified Control.Concurrent.FairRWLock as L


-- |
-- An in-memory graph with support for transactions without persistence.
data Graph u = Graph !(U.Node u) !(L.RWLock)

-- |
-- Create a graph providing a value of the root node.
new :: U.PolyValue u a => a -> IO (Graph u)
new root = Graph <$> initRoot <*> L.new
  where
    initRoot = pure (U.packValue root) >>= pure . snd >>= N.new

instance (U.Union u) => B.Backend (Graph u) where
  type Tx (Graph u) = ReaderT (Graph u) IO
  newtype Node (Graph u) = Node (U.Node u)
  newtype Value (Graph u) = Value (U.Value u)
  newtype Type (Graph u) = Type (U.Type u)
  newtype Index (Graph u) = Index (U.Index u)
  runRead tx g = let Graph _ l = g in L.withRead l $ runReaderT tx g
  runWrite tx g = let Graph _ l = g in L.withWrite l $ runReaderT tx g
  getRoot = do Graph root _ <- ask; return $ Node root
  getTargetsByType (Node n) (Type t) = liftIO $ N.getTargetsByType n t >>= return . map Node

instance (U.Union u) => Serializable IO (Graph u) where
  serialize (Graph n _) = serialize n
  deserialize = Graph <$> deserialize <*> liftIO L.new
