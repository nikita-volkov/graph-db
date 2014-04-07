module GraphDB.Session where

import GraphDB.Util.Prelude
import qualified GraphDB.FreeTransaction.Action as A
import qualified GraphDB.Session.TransactionLog as TL
import qualified GraphDB.Util.IOQueue as IOQueue
import qualified GraphDB.Model.Union as U 
import qualified GraphDB.Storage as S
import qualified GraphDB.Util.DIOVector as DIOVector



class Session s where
  type Settings s 
  type Result s r
  type Node s u
  runAction :: (Monad m) => Bool -> A.Action (Node s u) u r -> s u m r
  run :: (Monad m) => Settings s -> s u m r -> m (Result s r)



data Persistence inner = 
  Persistence !inner !(S.Storage inner (TL.Log inner)) !IOQueue.IOQueue

newtype PersistenceSession i u (m :: * -> *) r = 
  PersistenceSession (Monad (i u m) => StateT [TL.Entry u] (StateT Int (i u m)) r)
  deriving (Functor)

instance Monad (PersistenceSession i u m) where
  return a = PersistenceSession $ return $ a
  PersistenceSession a >>= k = PersistenceSession $
     a >>= return . k >>= \(PersistenceSession i) -> i

instance (Session i) => Session (PersistenceSession i) where
  type Settings (PersistenceSession i) = ()
  type Result (PersistenceSession i) r = r
  type Node (PersistenceSession i) u = Int
  runAction isWrite = iterM $ \case
    A.NewNode v c -> do
      PersistenceSession $ modify $ (:) $ TL.NewNode v
      ir <- PersistenceSession $ lift $ lift $ runAction isWrite $ liftF $ A.NewNode v id
      r <- newRef ir
      c r
    where
      newRef n = PersistenceSession $ lift $ do
        index <- get
        modify succ
        return $ index


