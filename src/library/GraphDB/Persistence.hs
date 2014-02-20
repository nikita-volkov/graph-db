module GraphDB.Persistence where

import GraphDB.Util.Prelude
import qualified GraphDB.Transaction.Tx as T
import qualified GraphDB.Union as U
import qualified GraphDB.Storage as S
import qualified GraphDB.Persistence.TransactionLog as TL
import qualified GraphDB.Graph as G

data Persistence u = Persistence (G.Graph u) (Storage u)
type Storage u = S.Storage (Maybe (U.Node u)) (TL.Log u)

instance T.Backend (Persistence u) where
  type Node (Persistence u) = (Int, U.Node u)
  type Value (Persistence u) = U.Value u
  type Type (Persistence u) = U.Type u
  type Index (Persistence u) = U.Index u


liftGraphTx :: T.Tx (G.Graph u) r -> T.Tx (Persistence u) r
liftGraphTx gtx = do
  Persistence g _ <- ask
  liftIO $ T.runTxReader gtx g

