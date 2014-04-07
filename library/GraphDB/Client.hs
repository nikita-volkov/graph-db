module GraphDB.Client where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Model.Union as U
import qualified Remotion.Client as Remo


-- * Session
-------------------------

newtype Session u m r = 
  Session (Remo.Client (Request u) (Response u) m r)
  deriving (Functor, Applicative, Monad, MonadIO)

type Request u = P.Request Node (U.Value u) (U.Type u) (U.Index u)
type Response u = P.Response Node (U.Value u) (U.Type u) (U.Index u)
type Node = Int

instance MonadTrans (Session u) where
  lift = Session . lift

type SessionSettings = Remo.Settings

runSession :: 
  (U.Serializable IO u, MonadBaseControl IO m, MonadIO m) => 
  SessionSettings -> Session u m r -> m (Either Remo.Failure r)
runSession settings (Session ses) = Remo.run settings ses


-- * Transaction
-------------------------

runTransaction :: (MonadIO m, Applicative m, U.Serializable IO u) => Bool -> Session u m r -> Session u m r
runTransaction write tx = do
  Session $ Remo.request $ P.Request_StartTransaction write
  r <- tx
  Session $ Remo.request $ P.Request_EndTransaction
  return r


-- * Action
-------------------------

type Action u = A.Action Node (U.Value u) (U.Type u) (U.Index u)

runAction :: (MonadIO m, Applicative m, U.Serializable IO u) => Action u m r -> Session u m r
runAction = iterTM $ \case
  A.GetTargetsByType n t c -> do
    r <- 
      Session $ Remo.request $ 
      P.Request_Transaction $ 
      P.Request_Transaction_Spec_GetTargetsByType n t
    case r of
      P.Response_Transaction (P.Response_Transaction_Spec_GetTargetsByType nl) -> c nl
      _ -> $bug "Unexpected response"
