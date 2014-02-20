module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Transaction.Backend as B


newtype Read b s r = Read (B.Tx b r)

instance Monad (B.Tx b) => Monad (Read b s) where
  return = Read . return
  Read a >>= k = Read $ a >>= return . k >>= \(Read b) -> b

instance Applicative (B.Tx b) => Applicative (Read b s) where 
  pure = Read . pure
  Read a <*> Read b = Read $ a <*> b

instance Functor (B.Tx b) => Functor (Read b s) where
  fmap f (Read a) = Read $ fmap f a


newtype Write b s r = Write (B.Tx b r)

instance Monad (B.Tx b) => Monad (Write b s) where
  return = Write . return
  Write a >>= k = Write $ a >>= return . k >>= \(Write b) -> b

instance Applicative (B.Tx b) => Applicative (Write b s) where 
  pure = Write . pure
  Write a <*> Write b = Write $ a <*> b

instance Functor (B.Tx b) => Functor (Write b s) where
  fmap f (Write a) = Write $ fmap f a


type ReadOrWrite b s r = forall t. (LiftTx t) => t b s r

class LiftTx t where liftTx :: B.Tx b r -> t b s r
instance LiftTx Read where liftTx = Read
instance LiftTx Write where liftTx = Write


-- | A transaction-local reference to node, which may not escape the transaction.
newtype Node b s = Node (B.Node b)

getRoot :: B.Backend b => ReadOrWrite b s (Node b s)
getRoot = liftTx $ B.getRoot >>= pure . Node

addTarget :: B.Backend b => Node b s -> Node b s -> Write b s Bool
addTarget (Node s) (Node t) = Write $ B.addTarget s t


runWrite :: B.Backend b => Write b s r -> b -> IO r
runWrite (Write tx) = B.runWrite tx

runRead :: B.Backend b => Read b s r -> b -> IO r
runRead (Read tx) = B.runRead tx

