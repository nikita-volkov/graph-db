module GraphDB.Session where

import GraphDB.Util.Prelude
import qualified GraphDB.FreeTransaction.Action as A
import qualified GraphDB.Session.TransactionLog as TL
import qualified GraphDB.Util.IOQueue as IOQueue
import qualified GraphDB.Model.Union as U 
import qualified GraphDB.Storage as S
import qualified GraphDB.Util.DIOVector as DIOVector



class (A.Tx (Tx s)) => Session s where
  type SessionResult s r
  type SessionSettings s 
  type Tx s
  runSession :: (Monad m) => SessionSettings s -> s u m r -> m (SessionResult s r)
  runTx :: (MonadIO m, Applicative m, U.Serializable IO u) => Bool -> Tx s u r -> s u m r


type Node s = A.Node (Tx s)

type Action s u = A.Action (Tx s) u
