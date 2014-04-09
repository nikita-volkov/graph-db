module GraphDB.Persistent where

import GraphDB.Util.Prelude

import qualified GraphDB.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as IOQueue
import qualified GraphDB.Storage as S
import qualified GraphDB.Nonpersistent as NP
import qualified GraphDB.Graph as Graph
import qualified GraphDB.Util.DIOVector as DV
import qualified GraphDB.Persistent.Log as L


-- * Session
-------------------------

-- |
-- A session of an in-memory graph datastructure with persistence.
newtype PersistentSession u m r = 
  PersistentSession (ReaderT (Storage u, IOQueue.IOQueue) (NP.NonpersistentSession u m) r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (PersistentSession u) where 
  lift = PersistentSession . lift . lift

instance MonadTransControl (PersistentSession u) where
  newtype StT (PersistentSession u) r = SessionStT (StT (NP.NonpersistentSession u) r)
  liftWith runInInner = do
    env <- PersistentSession $ ask
    PersistentSession $ lift $ liftWith $ \runGraphSession -> runInInner $ \(PersistentSession s) ->
      liftM SessionStT . runGraphSession . flip runReaderT env $ s

  restoreT inner = do
    PersistentSession $ lift $ do
      SessionStT r <- lift inner
      restoreT $ return $ r

instance (MonadBase IO m) => MonadBase IO (PersistentSession u m) where
  liftBase = PersistentSession . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (PersistentSession u m) where
  newtype StM (PersistentSession u m) a = SessionStM { unSessionStM :: ComposeSt (PersistentSession u) m a }
  liftBaseWith = defaultLiftBaseWith SessionStM
  restoreM = defaultRestoreM unSessionStM

type Storage u = S.Storage (NP.Node u) (L.Log u)

data PersistenceFailure = 
  -- | 
  -- Corrupt data during deserialization of the stored data.
  CorruptData Text

type Settings u = (U.Value u, StoragePath, PersistenceBuffering)
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
  (MonadIO m, MonadBaseControl IO m, U.Serializable IO u, U.Union u) => 
  Settings u -> PersistentSession u m r -> m (Either PersistenceFailure r)
runSession (v, p, buffering) (PersistentSession ses) = do
  r <- liftBaseWith $ \runInBase -> do
    let
      acquire = do
        paths <- S.pathsFromDirectory p
        (storage, graph) <- S.acquireAndLoad initGraph applyLog paths
        queue <- IOQueue.start buffering
        return (storage, queue, graph)
        where
          initGraph = Graph.new v
          applyLog graph log = do
            void $ runInBase $ NP.runSession graph $ NP.runAction $ L.toAction log
      release (storage, queue, graph) = do
        IOQueue.shutdown queue
        S.checkpoint storage graph
        S.release storage
    try $ bracket acquire release $ \(s, q, g) -> 
      runInBase $ NP.runSession g $ flip runReaderT (s, q) $ ses
  either (return . Left . adaptStorageException) (fmap Right . restoreM) r
  where
    adaptStorageException = \case
      S.DeserializationFailure t -> CorruptData t


-- * Transaction
-------------------------

newtype Tx u m r = 
  Tx (StateT (L.Log u) (StateT Int (NP.NonpersistentSession u m)) r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Tx u) where 
  lift = Tx . lift . lift . lift

runTransaction :: (MonadBaseControl IO m, U.Serializable IO u) => Bool -> Tx u m r -> PersistentSession u m r
runTransaction write (Tx tx) = PersistentSession $ do
  (r, log) <- 
    lift $ NP.runTransaction write $ flip evalStateT 0 $ flip runStateT [] $ tx
  when write $ do
    (storage, ioq) <- ask
    liftBase $ IOQueue.enqueue ioq $ S.persistEvent storage $ reverse log
  return r


-- * Action
-------------------------

type Action u = A.Action L.NodeRef (U.Value u) (U.Type u) (U.Index u)

runAction :: (MonadBase IO m, U.Union u) => Action u m r -> Tx u m r
runAction = iterTM $ \case
  A.NewNode v c -> do
    record $ L.NewNode v
    ir <- Tx $ lift $ lift $ NP.runAction $ A.newNode v
    r <- newRef ir
    c r
  where
    record e = Tx $ modify $ (:) e
    newRef n = Tx $ lift $ do
      index <- get
      modify succ
      return $ index

