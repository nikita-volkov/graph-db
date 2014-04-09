module GraphDB.Client where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Model.Union as U
import qualified Remotion.Client as R


-- * Session
-------------------------

newtype Session u m r = 
  Session (Client u m r)
  deriving (Functor, Applicative, Monad, MonadIO)

type Client u = R.Client (P.Request u) (P.Response u)

instance MonadTrans (Session u) where
  lift = Session . lift

instance MonadTransControl (Session u) where
  newtype StT (Session u) r = SessionStT (StT (Client u) r)
  liftWith runInInner =
    Session $ liftWith $ \runClient -> runInInner $ \(Session s) ->
    liftM SessionStT $ runClient $ s
  restoreT inner = do
    Session $ do
      SessionStT r <- lift $ inner
      restoreT $ return $ r

instance (MonadBase IO m) => MonadBase IO (Session u m) where
  liftBase = Session . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (Session u m) where
  newtype StM (Session u m) a = SessionStM { unSessionStM :: ComposeSt (Session u) m a }
  liftBaseWith = defaultLiftBaseWith SessionStM
  restoreM = defaultRestoreM unSessionStM

type SessionSettings = R.Settings

runSession :: 
  (U.Serializable IO u, MonadBaseControl IO m, MonadIO m) => 
  SessionSettings -> Session u m r -> m (Either R.Failure r)
runSession settings (Session ses) = R.run settings ses


-- * Transaction
-------------------------

runTransaction :: (MonadIO m, Applicative m, U.Serializable IO u) => Bool -> Session u m r -> Session u m r
runTransaction write tx = do
  Session $ R.request $ P.Start write
  r <- tx
  Session $ R.request $ P.Finish
  return r


-- * Action
-------------------------

type Action u = A.Action Int (U.Value u) (U.Type u) (U.Index u)

runAction :: (MonadIO m, Applicative m, U.Serializable IO u) => Action u m r -> Session u m r
runAction = iterTM $ \case
  A.GetTargetsByType n t c -> do
    r <- Session $ R.request $ P.Action $ P.GetTargetsByType n t
    case r of
      P.NodeList nl -> c nl
      _ -> $bug "Unexpected response"
