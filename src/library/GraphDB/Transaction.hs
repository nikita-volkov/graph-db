module GraphDB.Transaction where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Union as Union
import qualified GraphDB.Engine.Node as Node



-- The backend modules provide specific implementations.
-- They also have to discern the specific types of transactions in runners, e.g. "runWrite".

type family Ref (t :: * -> * -> * -> *) u s

class Read t where
  getRoot :: t u s (Ref t u s)
  getTargetsByType :: Union.Union u => Ref t u s -> Union.Type u -> t u s [Ref t u s]

class Write t where
  addTarget :: Ref t u s -> Ref t u s -> t u s Bool

-- Synonyms still require specifying the transaction type, because of "Ref t"
-- getRoot :: ReadOrWrite u s (Ref t u s)
-- type ReadOrWrite u s = forall t. (ReadOps t, MonadIO (t u s), Applicative (t u s)) => t u s
-- type Write u s = forall t. (ReadOps t, WriteOps t, MonadIO (t u s), Applicative (t u s)) => t u s



