module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Union as Union
import qualified GraphDB.Engine.Node as Node
import qualified GraphDB.Transaction.Tx as Tx


newtype Read u b s r = Read (Tx.Tx u b r) deriving (Monad, Functor, Applicative, MonadIO)
newtype Write u b s r = Write (Tx.Tx u b r) deriving (Monad, Functor, Applicative, MonadIO)
type ReadOrWrite u b s r = forall t. (LiftTx t) => t u b s r

class LiftTx t where liftTx :: Tx.Tx u b r -> t u b s r
instance LiftTx Read where liftTx = Read
instance LiftTx Write where liftTx = Write

-- | A transaction-local reference to node, which may not escape the transaction.
newtype Node u b s = Node (Tx.Node u b)

getRoot :: Tx.Backend u b => ReadOrWrite u b s (Node u b s)
getRoot = liftTx $ Tx.getRoot >>= pure . Node

addTarget :: Tx.Backend u b => Node u b s -> Node u b s -> Write u b s Bool
addTarget (Node s) (Node t) = Write $ Tx.addTarget s t

runWrite :: Tx.Backend u b => Write u b s r -> b -> IO r
runWrite (Write tx) = Tx.runTx tx True

runRead :: Tx.Backend u b => Read u b s r -> b -> IO r
runRead (Read tx) = Tx.runTx tx False

