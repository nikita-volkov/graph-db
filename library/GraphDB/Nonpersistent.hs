module GraphDB.Nonpersistent where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Graph as G
import qualified Control.Concurrent.FairRWLock as L


-- * Session 
-------------------------

type Node u = U.Node u

-- |
-- A session of an in-memory graph datastructure with no persistence.
newtype NonpersistentSession u m r = 
  NonpersistentSession { unSession :: ReaderT (U.Node u) (ReaderT L.RWLock m) r }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (NonpersistentSession u) where 
  lift = NonpersistentSession . lift . lift

instance MonadTransControl (NonpersistentSession u) where
  newtype StT (NonpersistentSession u) r = SessionStT r
  liftWith runInInner = do
    root <- NonpersistentSession $ ask
    lock <- NonpersistentSession $ lift $ ask
    NonpersistentSession $ lift $ lift $ runInInner $ 
      liftM SessionStT . flip runReaderT lock . flip runReaderT root . unSession
  restoreT inner = do
    SessionStT r <- NonpersistentSession $ lift $ lift $ inner
    return r

instance (MonadBase IO m) => MonadBase IO (NonpersistentSession u m) where
  liftBase = NonpersistentSession . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (NonpersistentSession u m) where
  newtype StM (NonpersistentSession u m) a = SessionStM { unSessionStM :: ComposeSt (NonpersistentSession u) m a }
  liftBaseWith = defaultLiftBaseWith SessionStM
  restoreM = defaultRestoreM unSessionStM

runSession :: (MonadIO m) => U.Node u -> NonpersistentSession u m r -> m r
runSession n (NonpersistentSession s) = do
  l <- liftIO $ L.new
  flip runReaderT l $ flip runReaderT n $ s


-- * Transaction
-------------------------

runTransaction :: (MonadBaseControl IO m) => Bool -> NonpersistentSession u m r -> NonpersistentSession u m r
runTransaction write tx = do
  l <- NonpersistentSession $ lift $ ask
  if write
    then control $ \runInBase -> L.withWrite l $ runInBase tx
    else control $ \runInBase -> L.withRead l $ runInBase tx


-- * Action
-------------------------

type Action u = A.Action (U.Node u) (U.Value u) (U.Type u) (U.Index u)

runAction :: (MonadBase IO m, U.Union u) => Action u m r -> NonpersistentSession u m r
runAction = iterTM $ \case
  A.NewNode v c -> liftBase (G.new v) >>= c
  A.GetValue n c -> liftBase (G.getValue n) >>= c
  A.SetValue n v c -> liftBase (G.setValue n v) >> c
  A.GetRoot c -> NonpersistentSession ask >>= c
  A.GetTargetsByType n t c -> liftBase (G.getTargetsByType n t) >>= c
  A.GetTargetsByIndex n i c -> liftBase (G.getTargetsByIndex n i) >>= c
  A.AddTarget s t c -> liftBase (G.addTarget s t) >>= c
  A.RemoveTarget s t c -> liftBase (G.removeTarget s t) >>= c
  A.Remove n c -> liftBase (G.remove n) >> c
  A.GetStats c -> NonpersistentSession ask >>= liftBase . G.getStats >>= c
