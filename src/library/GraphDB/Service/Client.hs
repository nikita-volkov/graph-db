module GraphDB.Service.Client where

import GraphDB.Util.Prelude 
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Transaction as T
import qualified GraphDB.Transaction.Backend as T
import qualified GraphDB.Union as U


data Client u

type Request u = P.Request Int (U.Value u) (U.Type u) (U.Index u)
type Response u = P.Response Int (U.Value u) (U.Type u) (U.Index u)

type Tx u = T.Tx (Client u)

instance T.Backend (Client u) where
  type Tx (Client u) = ReaderT (Client u) IO
  newtype Node (Client u) = Node Int
  newtype Value (Client u) = Value (U.Value u)
  newtype Type (Client u) = Type (U.Type u)
  newtype Index (Client u) = Index (U.Index u)
  runRead = txRunner False
  runWrite = txRunner True
  getRoot = runRequestAndParse parse request where
    parse = \case
      P.Response_Transaction (P.Response_Transaction_Spec_GetRoot r) -> Just $ Node r
      _ -> Nothing
    request = P.Request_Transaction $ P.Request_Transaction_Spec_GetRoot
  addTarget (Node s) (Node t) = runRequestAndParse parse request where
    parse = \case
      P.Response_Transaction (P.Response_Transaction_Spec_AddTarget r) -> Just r
      _ -> Nothing
    request = P.Request_Transaction $ P.Request_Transaction_Spec_AddTarget s t

runRequest :: Request u -> Tx u (Response u)
runRequest r = do
  client <- ask
  $notImplemented

runRequestAndParse :: (Response u -> Maybe r) -> Request u -> Tx u r
runRequestAndParse parseResponse request = 
  runRequest request >>= pure . parseResponse >>= pure . fromMaybe ($(bug "Unexpected response"))



txRunner :: Bool -> (ReaderT (Client u) IO r -> Client u -> IO r)
txRunner isWrite = \tx client -> flip runReaderT client $ do
  runRequest $ P.Request_StartTransaction isWrite
  r <- tx
  runRequest $ P.Request_EndTransaction
  return r

