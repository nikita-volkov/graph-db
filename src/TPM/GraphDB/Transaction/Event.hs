-- |
-- Transaction-tags allow logging of transactions.
module TPM.GraphDB.Transaction.Event where

import TPM.GraphDB.Prelude hiding (Read, Write)
import TPM.GraphDB.Transaction as Transaction
import qualified TPM.GraphDB.DB as DB; import TPM.GraphDB.DB (DB)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal



class Event e where
  type EventDB e
  type EventTransaction e
  type EventResult e
  transaction :: e -> (EventTransaction e) (EventDB e) s (EventResult e)

-- | Required for serialization for persistence.
data family UnionEvent db

class IsUnionEventOf e db where
  toUnionEvent :: e -> UnionEvent db
  fromUnionEvent :: UnionEvent db -> Maybe e



run :: 
  (Event e, Transaction (EventTransaction e), IsUnionEventOf e (EventDB e)) => 
  DB (EventDB e) -> e -> IO (EventResult e)
run db event = runTransaction <* persist where
  runTransaction = Transaction.run db $ transaction event
  persist = 
    trace "TODO: implement 'TPM.GraphDB.Transaction.Event.run.persist'" $
    return ()
