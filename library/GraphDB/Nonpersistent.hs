module GraphDB.Nonpersistent where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Graph.Node as N
import qualified Control.Concurrent.FairRWLock as L


-- * Session 
-------------------------

type Node u = U.Node u

newtype Session u m r = 
  Session { unSession :: ReaderT (U.Node u) (ReaderT L.RWLock m) r }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Session u) where 
  lift = Session . lift . lift

instance MonadTransControl (Session u) where
  newtype StT (Session u) r = SessionStT r
  liftWith runInInner = do
    root <- Session $ ask
    lock <- Session $ lift $ ask
    Session $ lift $ lift $ runInInner $ 
      liftM SessionStT . flip runReaderT lock . flip runReaderT root . unSession
  restoreT inner = do
    SessionStT r <- Session $ lift $ lift $ inner
    return r

instance (MonadBase IO m) => MonadBase IO (Session u m) where
  liftBase = Session . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (Session u m) where
  newtype StM (Session u m) a = SessionStM { unSessionStM :: ComposeSt (Session u) m a }
  liftBaseWith = defaultLiftBaseWith SessionStM
  restoreM = defaultRestoreM unSessionStM

runSession :: (MonadIO m) => U.Node u -> Session u m r -> m r
runSession n (Session s) = do
  l <- liftIO $ L.new
  flip runReaderT l $ flip runReaderT n $ s


-- * Transaction
-------------------------

runTransaction :: (MonadBaseControl IO m) => Bool -> Session u m r -> Session u m r
runTransaction write tx = do
  l <- Session $ lift $ ask
  if write
    then control $ \runInBase -> L.withWrite l $ runInBase tx
    else control $ \runInBase -> L.withRead l $ runInBase tx


-- * Action
-------------------------

type Action u = A.Action (U.Node u) (U.Value u) (U.Type u) (U.Index u)

runAction :: (MonadBase IO m, U.Union u) => Action u m r -> Session u m r
runAction = iterTM $ \case
  A.NewNode v c -> liftBase (N.new v) >>= c
  A.GetValue n c -> liftBase (N.getValue n) >>= c
  A.SetValue n v c -> liftBase (N.setValue n v) >> c
  A.GetRoot c -> Session ask >>= c
  A.GetTargetsByType n t c -> liftBase (N.getTargetsByType n t) >>= c
  A.GetTargetsByIndex n i c -> liftBase (N.getTargetsByIndex n i) >>= c
  A.AddTarget s t c -> liftBase (N.addTarget s t) >>= c
  A.RemoveTarget s t c -> liftBase (N.removeTarget s t) >>= c
  A.GetStats c -> Session ask >>= liftBase . N.getStats >>= c
