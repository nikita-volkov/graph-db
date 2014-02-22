module GraphDB.Graph
  ( 
    Graph,
    new,
  )
  where

import GraphDB.Util.Prelude
import qualified GraphDB.Transaction
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Model.Union as U
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
    initRoot = U.packValue root |> \(_, uv) -> N.new uv

instance (U.Union u) => B.Backend (Graph u) where
  type Tx (Graph u) = ReaderT (Graph u) IO
  newtype Node (Graph u) = Node (U.Node u)
  newtype Value (Graph u) = Value (U.Value u)
  newtype Type (Graph u) = Type (U.Type u)
  newtype Index (Graph u) = Index (U.Index u)
  runRead tx g = let Graph _ l = g in L.withRead l $ runReaderT tx g
  runWrite tx g = g |> \(Graph _ l) -> runReaderT tx g |> L.withWrite l
  newNode (Value uv) = N.new uv |> liftIO |> fmap Node
  getValue (Node un) = N.getValue un |> liftIO |> fmap Value
  setValue (Node un) (Value uv) = N.setValue un uv |> liftIO
  getRoot = do Graph root _ <- ask; return $ Node root
  getTargetsByType (Node n) (Type t) = liftIO $ N.getTargetsByType n t >>= return . map Node
  getTargetsByIndex (Node un) (Index ui) = N.getTargetsByIndex un ui |> liftIO |> fmap (map Node)
  addTarget (Node s) (Node t) = N.addTarget s t |> liftIO
  removeTarget (Node s) (Node t) = N.removeTarget s t |> liftIO
  getStats (Node n) = N.getStats n |> liftIO

instance (U.Union u) => Serializable IO (Graph u) where
  serialize (Graph n _) = serialize n
  deserialize = Graph <$> deserialize <*> liftIO L.new

instance U.PolyValue u v => B.PolyValue (Graph u) v where
  packValue v = U.packValue v |> \(ut, uv) -> (Type ut, Value uv)
  unpackValue (Value uv) = U.unpackValue uv

instance U.PolyIndex u i => B.PolyIndex (Graph u) i where
  packIndex i = U.packIndex i |> Index
