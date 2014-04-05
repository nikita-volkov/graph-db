module GraphDB.FreeTransaction where

import GraphDB.Util.Prelude hiding (Read, Write, read, write)
import Control.Monad.Free
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Model.Edge as E


data TxF u s n =
  forall v. U.PolyValue u v => NewNode v (Node s v -> n) |
  forall v. U.PolyValue u v => GetValue (Node s v) (v -> n) |
  forall v. U.PolyValue u v => SetValue (Node s v) v n |
  GetRoot (Node s u -> n)

instance Functor (TxF u s) where
  fmap f = \case 
    NewNode v c -> NewNode v (f . c)
    GetValue n c -> GetValue n (f . c)
    SetValue n v c -> SetValue n v (f c)
    GetRoot c -> GetRoot (f . c)

type Tx u s = Free (TxF u s)

-- NOTE: if we parameterize it with backend,
-- we can make it a data family, 
-- thus deferring its implementation to the backend.
data Node s v

newtype Write u s r = Write (Tx u s r) deriving (Functor, Applicative, Monad)
newtype Read u s r = Read (Tx u s r) deriving (Functor, Applicative, Monad)
-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite u s r = forall t. (LiftTx t, Monad (t u s), Applicative (t u s)) => t u s r

class LiftTx t where liftTx :: Tx u s r -> t u s r
instance LiftTx Read where liftTx = Read
instance LiftTx Write where liftTx = Write

