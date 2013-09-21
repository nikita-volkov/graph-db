{-# LANGUAGE UndecidableInstances #-}
-- |
-- Transaction-tags allow logging of transactions.
module TPM.GraphDB.Transaction.Tag where

import TPM.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.Transaction as Transaction
import qualified TPM.GraphDB.DB as DB; import TPM.GraphDB.DB (DB)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)

import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal



class Tag t where
  type Transaction t
  type TransactionResult t
  transaction :: t -> (Transaction t) s (TransactionResult t)

run :: (Tag t, Transaction.Transaction (Transaction t)) => DB -> t -> IO (TransactionResult t)
run db tag = Transaction.run db $ transaction tag




