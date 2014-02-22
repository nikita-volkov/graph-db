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

instance (U.Union u) => B.Backend (Graph u) u where
  newtype Tx (Graph u) u r = Tx (ReaderT (Graph u) IO r)
  newtype Node (Graph u) u = Node (U.Node u)
  runRead (Tx tx) g = let Graph _ l = g in L.withRead l $ runReaderT tx g
  runWrite (Tx tx) g = g |> \(Graph _ l) -> runReaderT tx g |> L.withWrite l
  newNode uv = N.new uv |> liftIO |> fmap Node
  getValue (Node un) = N.getValue un |> liftIO
  setValue (Node un) uv = N.setValue un uv |> liftIO
  getRoot = do Graph root _ <- Tx ask; return $ Node root
  getTargetsByType (Node un) ut = liftIO $ N.getTargetsByType un ut >>= return . map Node
  getTargetsByIndex (Node un) ui = N.getTargetsByIndex un ui |> liftIO |> fmap (map Node)
  addTarget (Node s) (Node t) = N.addTarget s t |> liftIO
  removeTarget (Node s) (Node t) = N.removeTarget s t |> liftIO
  getStats (Node n) = N.getStats n |> liftIO

instance (U.Union u) => Serializable IO (Graph u) where
  serialize (Graph n _) = serialize n
  deserialize = Graph <$> deserialize <*> liftIO L.new

-- Yeah. Redundant boilerplate, 
-- but GHC doesn't yet have the features required to elude it implemented.

instance MonadIO (B.Tx (Graph u) u) where
  liftIO = Tx . liftIO

instance Monad (B.Tx (Graph u) u) where
  return = Tx . return
  Tx a >>= k = Tx $ a >>= return . k >>= \(Tx b) -> b

instance Applicative (B.Tx (Graph u) u) where 
  pure = Tx . pure
  Tx a <*> Tx b = Tx $ a <*> b

instance Functor (B.Tx (Graph u) u) where
  fmap f (Tx a) = Tx $ fmap f a
