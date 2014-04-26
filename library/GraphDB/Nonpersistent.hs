module GraphDB.Nonpersistent where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Graph as G
import qualified Control.Concurrent.FairRWLock as L


-- * Session 
-------------------------

type Node s = G.Node s

-- |
-- A session of an in-memory graph datastructure with no persistence.
newtype NonpersistentSession s m r = 
  NonpersistentSession { unSession :: ReaderT (G.Node s) (ReaderT L.RWLock m) r }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (NonpersistentSession s) where 
  lift = NonpersistentSession . lift . lift

instance MonadTransControl (NonpersistentSession s) where
  newtype StT (NonpersistentSession s) r = SessionStT r
  liftWith runInInner = do
    root <- NonpersistentSession $ ask
    lock <- NonpersistentSession $ lift $ ask
    NonpersistentSession $ lift $ lift $ runInInner $ 
      liftM SessionStT . flip runReaderT lock . flip runReaderT root . unSession
  restoreT inner = do
    SessionStT r <- NonpersistentSession $ lift $ lift $ inner
    return r

instance (MonadBase IO m) => MonadBase IO (NonpersistentSession s m) where
  liftBase = NonpersistentSession . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (NonpersistentSession s m) where
  newtype StM (NonpersistentSession s m) a = SessionStM { unSessionStM :: ComposeSt (NonpersistentSession s) m a }
  liftBaseWith = defaultLiftBaseWith SessionStM
  restoreM = defaultRestoreM unSessionStM

runSession :: (MonadIO m) => G.Node s -> NonpersistentSession s m r -> m r
runSession n (NonpersistentSession s) = do
  l <- liftIO $ L.new
  flip runReaderT l $ flip runReaderT n $ s


-- * Transaction
-------------------------

runTransaction :: (MonadBaseControl IO m) => Bool -> NonpersistentSession s m r -> NonpersistentSession s m r
runTransaction write tx = do
  l <- NonpersistentSession $ lift $ ask
  if write
    then control $ \runInBase -> L.withWrite l $ runInBase tx
    else control $ \runInBase -> L.withRead l $ runInBase tx


-- * Action
-------------------------

type Action s = A.Action (G.Node s) (G.Value s) (G.Index s)

runAction :: (MonadIO m, G.Setup s) => Action s m r -> NonpersistentSession s m r
runAction = iterTM $ \case
  A.NewNode v c -> liftIO (G.new v) >>= c
  A.GetValue n c -> liftIO (G.getValue n) >>= c
  A.SetValue n v c -> liftIO (G.setValue n v) >> c
  A.GetRoot c -> NonpersistentSession ask >>= c
  A.GetTargetsByIndex n i c -> liftIO (G.getTargetsByIndex n i) >>= c
  A.AddTarget s t c -> liftIO (G.addTarget s t) >> c
  A.RemoveTarget s t c -> liftIO (G.removeTarget s t) >> c
  A.Remove n c -> liftIO (G.remove n) >> c
  A.GetStats c -> NonpersistentSession ask >>= liftIO . G.getStats >>= c
