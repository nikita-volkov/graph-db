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
import qualified GraphDB.Graph.Node as N
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
  type Union (Graph u) = u
  newtype Tx (Graph u) r = Tx (ReaderT (Graph u) IO r)
  type Node (Graph u) = U.Node u
  runRead (Tx tx) g = let Graph _ l = g in L.withRead l $ runReaderT tx g
  runWrite (Tx tx) g = g |> \(Graph _ l) -> runReaderT tx g |> L.withWrite l
  newNode uv = N.new uv |> liftIO
  getValue un = N.getValue un |> liftIO
  setValue un uv = N.setValue un uv |> liftIO
  getRoot = Tx ask >>= \(Graph r _) -> return r
  getTargetsByType un ut = liftIO $ N.getTargetsByType un ut
  getTargetsByIndex un ui = N.getTargetsByIndex un ui |> liftIO
  addTarget s t = N.addTarget s t |> liftIO
  removeTarget s t = N.removeTarget s t |> liftIO
  getStats un = N.getStats un |> liftIO

instance (U.Union u) => Serializable IO (Graph u) where
  serialize (Graph n _) = serialize n
  deserialize = Graph <$> deserialize <*> liftIO L.new

-- Yeah. Redundant boilerplate, 
-- but GHC doesn't yet have the features required to elude it implemented.

instance MonadIO (B.Tx (Graph u)) where
  liftIO = Tx . liftIO

instance Monad (B.Tx (Graph u)) where
  return = Tx . return
  Tx a >>= k = Tx $ a >>= return . k >>= \(Tx b) -> b

instance Applicative (B.Tx (Graph u)) where 
  pure = Tx . pure
  Tx a <*> Tx b = Tx $ a <*> b

instance Functor (B.Tx (Graph u)) where
  fmap f (Tx a) = Tx $ fmap f a
