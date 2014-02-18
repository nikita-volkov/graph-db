module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Storage as Storage
import qualified GraphDB.Union as Union
import qualified GraphDB.Engine.Node as Node
import qualified GraphDB.Transaction.Read as Read
import qualified GraphDB.Transaction.Write as Write
import qualified GraphDB.Transaction.Log as Log


type Read = Read.Read
type Write = Write.Write
type Log u = Log.Log u
type Storage u = Storage.Storage u (Log u)


-- | A packed uncomposable specific transaction with information about its type.
data Transaction u r =
  Write (forall s. Write u s r) |
  Read (forall s. Read u s r)

instance Functor (Transaction u) where
  fmap f = \case
    Read read -> Read $ fmap f read
    Write write -> Write $ fmap f write

-- |
-- Apply the transaction to the in-memory data-structure and
-- persist the updates if successful.
run :: (Serializable IO (Log u)) => Transaction u r -> (Union.Node u, Storage u) -> IO r
run transaction (root, storage) = case transaction of
  Read read -> Read.run read root
  Write write -> do
    (r, log) <- Write.run write root
    Storage.persistEvent storage log
    return r

runWrite :: Write u s r -> Union.Node u -> IO (r, Log u)
runWrite = Write.run

runRead :: Read u s r -> Union.Node u -> IO r
runRead = Read.run


-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite t u s = 
  (SharedOps t, MonadIO (t u s), Applicative (t u s)) => t u s

class SharedOps t where
  type Ref t u s
  getRoot :: t u s (Ref t u s)
  getTargetsByType :: (Union.Union u) => Ref t u s -> Union.Type u -> t u s [Ref t u s]

instance SharedOps Read where
  type Ref Read u s = Read.Ref u s
  getRoot = Read.getRoot
  getTargetsByType = Read.getTargetsByType

instance SharedOps Write where
  type Ref Write u s = Write.Ref u s
  getRoot = Write.getRoot
  getTargetsByType = Write.getTargetsByType

