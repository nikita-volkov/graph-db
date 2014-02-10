module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Storage as Storage
import qualified GraphDB.Transaction.Node as Node
import qualified GraphDB.Transaction.Read as Read
import qualified GraphDB.Transaction.Write as Write
import qualified GraphDB.Transaction.Log as Log


type Read = Read.Read
type Write = Write.Write
type Log n = Log.Log n
type Storage n = Storage.Storage n (Log n)


-- | A packed uncomposable specific transaction with information about its type.
data Transaction n r =
  Write (forall s. Write n s r) |
  Read (forall s. Read n s r)

instance Functor (Transaction n) where
  fmap f = \case
    Read read -> Read $ fmap f read
    Write write -> Write $ fmap f write

-- |
-- Apply the transaction to the in-memory data-structure and
-- persist the updates if successful.
run :: (Serializable IO (Log n)) => Transaction n r -> (n, Storage n) -> IO r
run transaction (root, storage) = case transaction of
  Read read -> Read.run read root
  Write write -> do
    (r, log) <- Write.run write root
    Storage.persistEvent storage log
    return r

runWrite :: Write n s r -> n -> IO (r, Log n)
runWrite = Write.run

runRead :: Read n s r -> n -> IO r
runRead = Read.run


-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite t n s = 
  (SharedOps t, MonadIO (t n s), Applicative (t n s)) => t n s

class SharedOps t where
  type Ref t n s
  getRoot :: t n s (Ref t n s)
  getTargetsByType :: (Node.Node n) => Ref t n s -> Node.Type n -> t n s [Ref t n s]

instance SharedOps Read where
  type Ref Read n s = Read.Ref n s
  getRoot = Read.getRoot
  getTargetsByType = Read.getTargetsByType

instance SharedOps Write where
  type Ref Write n s = Write.Ref n s
  getRoot = Write.getRoot
  getTargetsByType = Write.getTargetsByType

