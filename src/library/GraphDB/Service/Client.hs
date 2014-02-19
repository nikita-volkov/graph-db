module GraphDB.Service.Client where

import GraphDB.Util.Prelude 
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Transaction as T
import qualified GraphDB.Union as U


data Client u

type Request u = P.Request Int (U.Value u) (U.Type u) (U.Index u)
type Response u = P.Response Int (U.Value u) (U.Type u) (U.Index u)


newtype Tx u r = 
  Tx (ReaderT (Client u) IO r)
  deriving (Functor, Applicative, Monad, MonadIO)

type Node = Int

getRoot :: Tx u Node
getRoot = do
  response <- request $ P.Request_Transaction $ P.Request_Transaction_Spec_GetRoot
  case response of
    P.Response_Transaction (P.Response_Transaction_Spec_GetRoot r) -> return r
    _ -> $(bug "Unexpected response")

addTarget :: Node -> Node -> Tx u Bool
addTarget s t = do
  response <- request $ P.Request_Transaction $ P.Request_Transaction_Spec_AddTarget s t
  case response of
    P.Response_Transaction (P.Response_Transaction_Spec_AddTarget r) -> return r
    _ -> $(bug "Unexpected response")

request :: Request u -> Tx u (Response u)
request r = do
  client <- Tx $ ask
  $notImplemented

runTx :: Tx u r -> Client u -> IO r
runTx (Tx reader) = runReaderT reader



newtype Write u s r = Write (Tx u r) deriving (Functor, Applicative, Monad, MonadIO)

type instance T.Ref Write u s = Int
instance T.Read Write where
  getRoot = Write $ getRoot

runWrite :: Write u s r -> Client u -> IO r
runWrite (Write tx) = runTx $ do
  request $ P.Request_StartTransaction True
  r <- tx
  request $ P.Request_EndTransaction
  return r






