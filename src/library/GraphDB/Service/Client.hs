module GraphDB.Service.Client where

import GraphDB.Util.Prelude 
import qualified GraphDB.Service.Protocol as P
import qualified GraphDB.Transaction as T
import qualified GraphDB.Transaction.Tx as T
import qualified GraphDB.Union as U


data Client u

type Request u = P.Request Int (U.Value u) (U.Type u) (U.Index u)
type Response u = P.Response Int (U.Value u) (U.Type u) (U.Index u)

type Tx u = T.Tx (Client u)

instance T.Backend (Client u) where
  type Node (Client u) = Int
  type Value (Client u) = U.Value u
  type Type (Client u) = U.Type u
  type Index (Client u) = U.Index u
  runTx tx isWrite client = 
    flip T.runTxReader client $ do
      runRequest $ P.Request_StartTransaction isWrite
      r <- tx
      runRequest $ P.Request_EndTransaction
      return r
  getRoot = runRequestAndParse parse request where
    parse = \case
      P.Response_Transaction (P.Response_Transaction_Spec_GetRoot r) -> Just r
      _ -> Nothing
    request = P.Request_Transaction $ P.Request_Transaction_Spec_GetRoot
  addTarget s t = runRequestAndParse parse request where
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





