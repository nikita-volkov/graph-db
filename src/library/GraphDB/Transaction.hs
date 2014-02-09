module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Storage as Storage
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)
import qualified GraphDB.Transaction.Read as Read
import qualified GraphDB.Transaction.Write as Write
import qualified GraphDB.Transaction.Log as Log


type Read = Read.Read
type Write = Write.Write
type Log t = Log.Log t
type Storage t = Storage.Storage (Node t) (Log t)


-- | A packed uncomposable specific transaction with information about its type.
data FinalTransaction t r =
  FinalTransaction_Write (forall s. Write t s r) |
  FinalTransaction_Read (forall s. Read t s r)

instance Functor (FinalTransaction t) where
  fmap f = \case
    FinalTransaction_Read read -> FinalTransaction_Read $ fmap f read
    FinalTransaction_Write write -> FinalTransaction_Write $ fmap f write

-- |
-- Apply the transaction to the in-memory data-structure and
-- persist the updates if successful.
runFinalTransaction :: (Serializable IO (Log t)) => FinalTransaction t r -> (Node t, Storage t) -> IO r
runFinalTransaction transaction (root, storage) = case transaction of
  FinalTransaction_Read read -> Read.run read root
  FinalTransaction_Write write -> do
    (r, log) <- Write.run write root
    Storage.persistEvent storage log
    return r


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

