-- | A log of persistence events. Represents a single log-file.
module GraphDB.Persistence.Log where

import GraphDB.Util.Prelude
import qualified GraphDB.Persistence.Log.Transaction as Transaction; import GraphDB.Persistence.Log.Transaction (Transaction)

data Log t

-- | Atomically log all actions of the transaction.
persist :: Log t -> Transaction t -> IO ()
persist = $notImplemented
