module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Persistence.Log as Log; import GraphDB.Persistence.Log (Log)
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)
import qualified GraphDB.Transaction.Read as Read; import GraphDB.Transaction.Read (Read)
import qualified GraphDB.Transaction.Write as Write; import GraphDB.Transaction.Write (Write)

-- | A packed uncomposable specific transaction with information about its type.
newtype Transaction t r = Transaction (forall s. Either (Read t s r) (Write t s r))

-- |
-- Apply the transaction to the in-memory data-structure and
-- persist the updates if successful.
runTransaction :: Transaction t r -> (Node t, Log t) -> IO r
runTransaction transaction (root, log) = case transaction of
  Transaction (Left read) -> Read.run read root
  Transaction (Right write) -> Write.run write (root, log)

-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite tr t s = 
  (SharedOps tr, MonadIO (tr t s), Applicative (tr t s)) => tr t s

class SharedOps tr where
  type Ref tr t s
  newRef :: Node t -> tr t s (Ref tr t s)
  getRoot :: tr t s (Ref tr t s)
  getTargetsByType :: (Node.Type t) => Ref tr t s -> t -> tr t s [Ref tr t s]

instance SharedOps Read where
  type Ref Read t s = Read.Ref t s
  newRef = Read.newRef
  getRoot = Read.getRoot
  getTargetsByType = Read.getTargetsByType

instance SharedOps Write where
  type Ref Write t s = Write.Ref t s
  newRef = Write.newRef
  getRoot = Write.getRoot
  getTargetsByType = Write.getTargetsByType

