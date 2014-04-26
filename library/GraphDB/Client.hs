module GraphDB.Client where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Protocol as P
import qualified GraphDB.Graph as G
import qualified Remotion.Client as RC


-- * Session
-------------------------

-- | 
-- A session of communication with server.
newtype ClientSession s m r = 
  ClientSession (Client s m r)
  deriving (Functor, Applicative, Monad, MonadIO)

type Client s = RC.Client (P.Request s) (P.Response s)

instance MonadTrans (ClientSession s) where
  lift = ClientSession . lift

instance MonadTransControl (ClientSession s) where
  newtype StT (ClientSession s) r = ClientSessionStT (StT (Client s) r)
  liftWith runInInner =
    ClientSession $ liftWith $ \runClient -> runInInner $ \(ClientSession s) ->
    liftM ClientSessionStT $ runClient $ s
  restoreT inner = do
    ClientSession $ do
      ClientSessionStT r <- lift $ inner
      restoreT $ return $ r

instance (MonadBase IO m) => MonadBase IO (ClientSession s m) where
  liftBase = ClientSession . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (ClientSession s m) where
  newtype StM (ClientSession s m) a = ClientSessionStM { unClientSessionStM :: ComposeSt (ClientSession s) m a }
  liftBaseWith = defaultLiftBaseWith ClientSessionStM
  restoreM = defaultRestoreM unClientSessionStM

type ClientSessionSettings = RC.Settings

runSession :: 
  (Serializable IO (G.Value s), Serializable IO (G.Index s), MonadBaseControl IO m, MonadIO m) => 
  ClientSessionSettings -> ClientSession s m r -> m (Either RC.Failure r)
runSession settings (ClientSession ses) = RC.run settings ses


-- * Transaction
-------------------------

runTransaction :: 
  (MonadIO m, Applicative m, Serializable IO (G.Value s), Serializable IO (G.Index s)) => 
  Bool -> ClientSession s m r -> ClientSession s m r
runTransaction write tx = do
  ClientSession $ RC.request $ P.Start write
  r <- tx
  ClientSession $ RC.request $ P.Finish
  return r


-- * Action
-------------------------

type Action s = A.Action Int (G.Value s) (G.Index s)

runAction :: 
  (MonadIO m, Applicative m, Serializable IO (G.Value s), Serializable IO (G.Index s)) => 
  Action s m r -> ClientSession s m r
runAction = iterTM $ \case
  A.NewNode v c -> do
    r <- req $ P.NewNode v
    case r of
      P.Node n -> c n
      _ -> $bug "Unexpected response"
  A.GetValue n c -> do
    r <- req $ P.GetValue n
    case r of
      P.Value v -> c v
      _ -> $bug "Unexpected response"
  A.SetValue n v c -> do
    r <- req $ P.SetValue n v
    case r of
      P.Unit -> c
      _ -> $bug "Unexpected response"
  A.GetRoot c -> do
    r <- req $ P.GetRoot
    case r of
      P.Node n -> c n
      _ -> $bug "Unexpected response"
  A.GetTargetsByIndex n i c -> do
    r <- req $ P.GetTargetsByIndex n i
    case r of
      P.NodeList nl -> c nl
      _ -> $bug "Unexpected response"
  A.AddTarget s t c -> do
    r <- req $ P.AddTarget s t
    case r of
      P.Unit -> c
      _ -> $bug "Unexpected response"
  A.RemoveTarget s t c -> do
    r <- req $ P.RemoveTarget s t
    case r of
      P.Unit -> c
      _ -> $bug "Unexpected response"
  A.Remove n c -> do
    (req $ P.Remove n) >>= \case
      P.Unit -> c
      _ -> $bug "Unexpected response"
  A.GetStats c -> do
    r <- req $ P.GetStats
    case r of
      P.Stats r -> c r
      _ -> $bug "Unexpected response"
  where
    req = ClientSession . RC.request . P.Action

