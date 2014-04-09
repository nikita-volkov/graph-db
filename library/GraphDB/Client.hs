module GraphDB.Client where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Model.Union as U
import qualified Remotion.Client as R


-- * Session
-------------------------

-- | 
-- A session of communication with server.
newtype ClientSession u m r = 
  ClientSession (Client u m r)
  deriving (Functor, Applicative, Monad, MonadIO)

type Client u = R.Client (P.Request u) (P.Response u)

instance MonadTrans (ClientSession u) where
  lift = ClientSession . lift

instance MonadTransControl (ClientSession u) where
  newtype StT (ClientSession u) r = ClientSessionStT (StT (Client u) r)
  liftWith runInInner =
    ClientSession $ liftWith $ \runClient -> runInInner $ \(ClientSession s) ->
    liftM ClientSessionStT $ runClient $ s
  restoreT inner = do
    ClientSession $ do
      ClientSessionStT r <- lift $ inner
      restoreT $ return $ r

instance (MonadBase IO m) => MonadBase IO (ClientSession u m) where
  liftBase = ClientSession . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (ClientSession u m) where
  newtype StM (ClientSession u m) a = ClientSessionStM { unClientSessionStM :: ComposeSt (ClientSession u) m a }
  liftBaseWith = defaultLiftBaseWith ClientSessionStM
  restoreM = defaultRestoreM unClientSessionStM

type ClientSessionSettings = R.Settings

runSession :: 
  (U.Serializable IO u, MonadBaseControl IO m, MonadIO m) => 
  ClientSessionSettings -> ClientSession u m r -> m (Either R.Failure r)
runSession settings (ClientSession ses) = R.run settings ses


-- * Transaction
-------------------------

runTransaction :: 
  (MonadIO m, Applicative m, U.Serializable IO u) => 
  Bool -> ClientSession u m r -> ClientSession u m r
runTransaction write tx = do
  ClientSession $ R.request $ P.Start write
  r <- tx
  ClientSession $ R.request $ P.Finish
  return r


-- * Action
-------------------------

type Action u = A.Action Int (U.Value u) (U.Type u) (U.Index u)

runAction :: 
  (MonadIO m, Applicative m, U.Serializable IO u) => 
  Action u m r -> ClientSession u m r
runAction = iterTM $ \case
  A.GetTargetsByType n t c -> do
    r <- ClientSession $ R.request $ P.Action $ P.GetTargetsByType n t
    case r of
      P.NodeList nl -> c nl
      _ -> $bug "Unexpected response"
