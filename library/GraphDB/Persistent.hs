module GraphDB.Persistent where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Graph as G
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as Q
import qualified GraphDB.Storage as S
import qualified GraphDB.Nonpersistent as NP
import qualified GraphDB.Persistent.Log as L


-- * Session
-------------------------

-- |
-- A session of an in-memory graph datastructure with persistence.
newtype PersistentSession s m r = 
  PersistentSession (ReaderT (Storage s, Q.IOQueue) (NP.NonpersistentSession s m) r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (PersistentSession s) where 
  lift = PersistentSession . lift . lift

instance MonadTransControl (PersistentSession s) where
  newtype StT (PersistentSession s) r = SessionStT (StT (NP.NonpersistentSession s) r)
  liftWith runInInner = do
    env <- PersistentSession $ ask
    PersistentSession $ lift $ liftWith $ \runGraphSession -> runInInner $ \(PersistentSession s) ->
      liftM SessionStT . runGraphSession . flip runReaderT env $ s

  restoreT inner = do
    PersistentSession $ lift $ do
      SessionStT r <- lift inner
      restoreT $ return $ r

instance (MonadBase IO m) => MonadBase IO (PersistentSession s m) where
  liftBase = PersistentSession . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (PersistentSession s m) where
  newtype StM (PersistentSession s m) a = SessionStM { unSessionStM :: ComposeSt (PersistentSession s) m a }
  liftBaseWith = defaultLiftBaseWith SessionStM
  restoreM = defaultRestoreM unSessionStM

type Storage s = S.Storage (NP.Node s) (L.Log s)

data PersistenceFailure = 
  -- | 
  -- Corrupt data during deserialization of the stored data.
  CorruptData Text
  deriving (Show)

type Settings s = (G.Value s, StoragePath, PersistenceBuffering)
-- |
-- A path to a directory, under which update-logs, checkpoints and archive 
-- will be stored.
-- 
-- The path will be interpreted, 
-- so you can use a tilde (@~@) character to refer to user's home directory.
type StoragePath = FilePath
-- |
-- An admissible amount of transactions,
-- by which the persistence layer may be lagging behind the actual state of the graph. 
-- Until that amount is reached the persistence of transactions is done asynchronously,
-- thus reducing the time of their execution and of acquisition of related locks.
-- If you want the persisted state to always accomodate to the actual in-memory state,
-- set this to @1@. 
-- Thus you can make sure that the persistence of updates is always done synchronously.
type PersistenceBuffering = Int

runSession :: 
  (MonadIO m, MonadBaseControl IO m, G.Setup s, Serializable IO (G.Index s)) => 
  Settings s -> PersistentSession s m r -> m (Either PersistenceFailure r)
runSession (v, p, buffering) (PersistentSession ses) = do
  r <- liftBaseWith $ \runInBase -> do
    let
      acquire = do
        paths <- S.pathsFromDirectory p
        (storage, graph) <- S.acquireAndLoad initGraph applyLog paths
        queue <- Q.start buffering
        return (storage, queue, graph)
        where
          initGraph = G.new v
          applyLog graph log = do
            void $ runInBase $ NP.runSession graph $ NP.runAction $ L.toAction log
      release (storage, queue, graph) = do
        Q.finish queue
        S.checkpoint storage graph
        S.release storage
    try $ bracket acquire release $ \(s, q, g) -> do
      runInBase $ NP.runSession g $ flip runReaderT (s, q) $ ses
  either (return . Left . adaptStorageException) (fmap Right . restoreM) r
  where
    adaptStorageException = \case
      S.DeserializationFailure t -> CorruptData t


-- * Transaction
-------------------------

newtype Tx s m r = 
  Tx (StateT (L.Log s) (StateT Int (NP.NonpersistentSession s m)) r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Tx s) where 
  lift = Tx . lift . lift . lift

runTransaction :: 
  (MonadBaseControl IO m, Serializable IO (G.Value s), Serializable IO (G.Index s)) => 
  Bool -> Tx s m r -> PersistentSession s m r
runTransaction write (Tx tx) = PersistentSession $ do
  (r, log) <- 
    lift $ NP.runTransaction write $ flip evalStateT 0 $ flip runStateT [] $ tx
  when write $ do
    (storage, ioq) <- ask
    liftBase $ Q.performAsync ioq $ S.persistEvent storage $ reverse log
  return r


-- * Action
-------------------------

type Action s = A.Action (Node s) (G.Value s) (G.Index s)
type Node s = (G.Node s, Int)

runAction :: (MonadIO m, G.Setup s) => Action s m r -> Tx s m r
runAction = iterTM $ \case
  A.NewNode v c -> do
    record $ L.NewNode v
    n <- runInner $ A.newNode v
    r <- newRef n
    c (n, r)
  A.GetValue (n, r) c -> do
    v <- runInner $ A.getValue n
    c v    
  A.SetValue (n, r) v c -> do
    record $ L.SetValue r v
    runInner $ A.setValue n v
    c
  A.GetRoot c -> do
    record $ L.GetRoot
    n <- runInner $ A.getRoot
    r <- newRef n
    c (n, r)
  A.GetTargetsByIndex (n, r) i c -> do
    record $ L.GetTargetsByIndex r i
    rns <- runInner $ A.getTargetsByIndex n i
    r <- forM rns $ \n -> liftM (n,) $ newRef n
    c r
  A.AddTarget (sn, sr) (tn, tr) c -> do
    record $ L.AddTarget sr tr
    runInner $ A.addTarget sn tn
    c
  A.RemoveTarget (sn, sr) (tn, tr) c -> do
    record $ L.RemoveTarget sr tr
    runInner $ A.removeTarget sn tn
    c
  A.Remove (n, r) c -> do
    record $ L.Remove r
    runInner $ A.remove n
    c
  A.GetStats c -> do
    r <- runInner $ A.getStats
    c r
  where
    record e = Tx $ modify $ (:) e
    newRef n = Tx $ lift $ do
      index <- get
      modify succ
      return $ index
    runInner = Tx . lift . lift . NP.runAction

