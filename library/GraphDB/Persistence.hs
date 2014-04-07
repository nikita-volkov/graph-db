{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence where

import GraphDB.Util.Prelude

import qualified GraphDB.Session as S
import qualified GraphDB.FreeTransaction.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as Q
import qualified GraphDB.Storage as S'


type Ref = Int

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry u =
  GetRoot |
  NewNode (U.Value u) |
  GetTargetsByType Ref (U.Type u) |
  GetTargetsByIndex Ref (U.Index u) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  SetValue Ref (U.Value u)
  deriving (Generic)

instance (U.Serializable m u) => Serializable m (Entry u)


-- |
-- A persistence wrapper backend for any other serializable backend,
-- e.g., an in-memory graph.
-- data Persistence inner = 
--   Persistence !inner !(S.Storage inner [Entry u]) !Q.IOQueue

data PersistenceSession i u m r =
  PersistenceSession 
    ( 
      (MonadIO (i u m), Monad (S.Tx i u), U.Serializable IO u) =>
      ReaderT 
        (S'.Storage (InnerSessionData i) [Entry u]) 
        (ReaderT Q.IOQueue (i u m)) 
        r
    )

instance Monad (PersistenceSession i u m) where
  return a = PersistenceSession $ return $ a
  PersistenceSession a >>= k = PersistenceSession $ a >>= return . k >>= \(PersistenceSession i) -> i

instance MonadIO (PersistenceSession i u m) where
  liftIO a = PersistenceSession $ liftIO a

instance (S.Session i) => S.Session (PersistenceSession i) where
  type Tx (PersistenceSession i) = PersistenceTx (S.Tx i)
  runTx isWrite (PersistenceTx m) = do
    (r, log) <- 
      PersistenceSession $ lift $ lift $ 
      S.runTx isWrite $ flip evalStateT 0 $ flip runStateT [] $ m
    when isWrite $ do
      storage <- PersistenceSession $ ask
      ioq <- PersistenceSession $ lift $ ask
      liftIO $ Q.enqueue ioq $ S'.persistEvent storage $ reverse log
    return r

class (Serializable IO (InnerSessionData s)) => InnerSession (s :: * -> (* -> *) -> * -> *) where
  -- | The data structure managed by the inner session.
  type InnerSessionData s
  initData :: IO (InnerSessionData s)

-- instance (InnerSession i) => InnerSession (PersistenceSession i) where
--   type InnerSessionData (PersistenceSession i) = InnerSessionData i
--   initData = PersistenceSession $ lift $ lift $ initData




newtype PersistenceTx itx u r = 
  PersistenceTx ((Monad (itx u)) => StateT [Entry u] (StateT Int (itx u)) r)

instance Monad (PersistenceTx itx u) where
  return a = PersistenceTx $ return $ a
  PersistenceTx a >>= k = PersistenceTx $ a >>= return . k >>= \(PersistenceTx i) -> i

instance (A.Tx itx) => A.Tx (PersistenceTx itx) where
  type Node (PersistenceTx itx) = Ref
  runAction = iterM $ \case
    A.NewNode v c -> do
      record $ NewNode v
      ir <- PersistenceTx $ lift $ lift $ A.runAction $ liftF $ A.NewNode v id
      r <- newRef ir
      c r
    where
      record e = PersistenceTx $ modify $ (:) e
      newRef n = PersistenceTx $ lift $ do
        index <- get
        modify succ
        return $ index


