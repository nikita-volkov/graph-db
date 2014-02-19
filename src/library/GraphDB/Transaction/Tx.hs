module GraphDB.Transaction.Tx where

import GraphDB.Util.Prelude
import qualified GraphDB.Union as U


-- | 
-- A low level transaction which both Read and Write revolve around.
newtype Tx u b r = Tx (ReaderT b IO r)
  deriving (Monad, Functor, Applicative, MonadIO, MonadReader b)

runTxReader :: Tx u b r -> b -> IO r
runTxReader (Tx reader) = runReaderT reader


class Backend u b | b -> u where
  type Node u b
  runTx :: Tx u b r 
        -> Bool -- ^ Is it a write?
        -> b
        -> IO r
  getRoot :: Tx u b (Node u b)
  getValue :: Node u b -> Tx u b (U.Value u)
  setValue :: Node u b -> U.Value u -> Tx u b ()
  getTargetsByType :: Node u b -> U.Type u -> Tx u b [Node u b]
  addTarget :: Node u b -> Node u b -> Tx u b Bool
  removeTarget :: Node u b -> Node u b -> Tx u b Bool
  getStats :: Node u b -> Tx u b (Int, Int)


class Monad m => MonadTx m where
  type MonadTx_Union m
  type MonadTx_Backend m
  liftTx :: Tx (MonadTx_Union m) (MonadTx_Backend m) r -> m r

instance MonadTx (Tx u b) where
  type MonadTx_Backend (Tx u b) = b
  type MonadTx_Union (Tx u b) = u
  liftTx = id
