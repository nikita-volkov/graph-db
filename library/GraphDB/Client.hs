module GraphDB.Client where

import GraphDB.Util.Prelude

import qualified GraphDB.Session as S
import qualified GraphDB.FreeTransaction.Action as A
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Model.Union as U
import qualified Remotion.Client as Remo


type Node = Int

type Request u = P.Request Node (U.Value u) (U.Type u) (U.Index u)

type Response u = P.Response Node (U.Value u) (U.Type u) (U.Index u)


newtype ClientSession u m r = 
  ClientSession (U.Serializable IO u => Remo.Client (Request u) (Response u) m r)

instance (Monad m) => Monad (ClientSession u m) where
  return a = ClientSession $ return $ a
  ClientSession a >>= k = ClientSession $ a >>= return . k >>= \case ClientSession b -> b

instance (MonadIO m) => MonadIO (ClientSession u m) where
  liftIO a = ClientSession $ liftIO $ a

instance S.Session ClientSession where
  type Tx ClientSession = ClientTx
  runTx isWrite (ClientTx s) = do
    ClientSession $ Remo.request $ P.Request_StartTransaction isWrite
    r <- s
    ClientSession $ Remo.request $ P.Request_EndTransaction
    return r


newtype ClientTx u r =
  ClientTx (forall m. (MonadIO m, Applicative m) => ClientSession u m r)

instance Monad (ClientTx u) where
  return a = ClientTx $ return $ a
  ClientTx a >>= k = ClientTx $ a >>= return . k >>= \case ClientTx b -> b

instance A.Tx ClientTx where
  type Node ClientTx = Node
  runAction = iterM $ \case
    A.GetTargetsByType n t c -> do
      r <- 
        ClientTx $ ClientSession $ Remo.request $ 
        P.Request_Transaction $ 
        P.Request_Transaction_Spec_GetTargetsByType n t
      case r of
        P.Response_Transaction (P.Response_Transaction_Spec_GetTargetsByType nl) -> c nl
        _ -> $bug "Unexpected response"

