-- |
-- Transaction-tags allow logging of transactions.
module TPM.GraphDB.Transaction.Event where

import TPM.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.Transaction as Transaction
import qualified TPM.GraphDB.DB as DB; import TPM.GraphDB.DB (DB)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)

import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal



class Event t where
  type DBTag t
  type Transaction t
  type Result t
  transaction :: t -> (Transaction t) (DBTag t) s (Result t)

run :: (Event t, Transaction.Transaction (Transaction t)) => DB (DBTag t) -> t -> IO (Result t)
run db tag = Transaction.run db $ transaction tag




