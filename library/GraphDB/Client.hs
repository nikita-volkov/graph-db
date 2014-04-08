module GraphDB.Client where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Model.Union as U
import qualified Remotion.Client as R


-- * Session
-------------------------

newtype Session u m r = 
  Session (R.Client (P.Request u) (P.Response u) m r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Session u) where
  lift = Session . lift

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
