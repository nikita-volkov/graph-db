module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Transaction.Tx as Tx


newtype Read b s r = Read (Tx.Tx b r) deriving (Monad, Functor, Applicative, MonadIO)
newtype Write b s r = Write (Tx.Tx b r) deriving (Monad, Functor, Applicative, MonadIO)
type ReadOrWrite b s r = forall t. (LiftTx t) => t b s r

class LiftTx t where liftTx :: Tx.Tx b r -> t b s r
instance LiftTx Read where liftTx = Read
instance LiftTx Write where liftTx = Write

-- | A transaction-local reference to node, which may not escape the transaction.
newtype Node b s = Node (Tx.Node b)

getRoot :: Tx.Backend b => ReadOrWrite b s (Node b s)
getRoot = liftTx $ Tx.getRoot >>= pure . Node

addTarget :: Tx.Backend b => Node b s -> Node b s -> Write b s Bool
addTarget (Node s) (Node t) = Write $ Tx.addTarget s t

runWrite :: Tx.Backend b => Write b s r -> b -> IO r
runWrite (Write tx) = Tx.runTx tx True

runRead :: Tx.Backend b => Read b s r -> b -> IO r
runRead (Read tx) = Tx.runTx tx False

