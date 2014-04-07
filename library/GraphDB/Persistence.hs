{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence where

import GraphDB.Util.Prelude

import qualified GraphDB.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as Q
import qualified GraphDB.Storage as S'
import qualified GraphDB.Graph as G


-- * Log
-------------------------

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry u =
  GetRoot |
  NewNode (U.Value u) |
  GetTargetsByType NodeRef (U.Type u) |
  GetTargetsByIndex NodeRef (U.Index u) |
  AddTarget NodeRef NodeRef |
  RemoveTarget NodeRef NodeRef |
  SetValue NodeRef (U.Value u)
  deriving (Generic)

instance (U.Serializable m u) => Serializable m (Entry u)

type NodeRef = Int


-- * Session
-------------------------

type Session u m = ReaderT (Storage u, Q.IOQueue) (G.Session u m)
type Storage u = S'.Storage (G.Node u) [Entry u]


-- * Transaction
-------------------------

type Tx u m r = StateT [Entry u] (StateT Int (G.Session u m)) r

runTransaction :: (MonadBaseControl IO m, U.Serializable IO u) => Bool -> Tx u m r -> Session u m r
runTransaction write tx = do
  (r, log) <- 
    lift $ G.runTransaction write $ flip evalStateT 0 $ flip runStateT [] $ tx
  when write $ do
    (storage, ioq) <- ask
    liftBase $ Q.enqueue ioq $ S'.persistEvent storage $ reverse log
  return r


-- * Action
-------------------------

type Action u r = A.Action NodeRef u r

runAction :: (MonadBase IO m, U.Union u) => Action u r -> Tx u m r
runAction = iterM $ \case
  A.NewNode v c -> do
    record $ NewNode v
    ir <- lift $ lift $ G.runAction $ liftF $ A.NewNode v id
    r <- newNode ir
    c r
  where
    record e = modify $ (:) e
    newNode n = lift $ do
      index <- get
      modify succ
      return $ index
