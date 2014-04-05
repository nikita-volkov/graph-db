{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence
  (
    -- * DB
    Persistence,
    with,
    -- ** Settings
    Settings,
    BufferSize,
    S.Paths,
    InitBackend,
    pathsFromName,
    S.pathsFromDirectory,
  )
  where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as IOQueue
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Model.Union as U 
import qualified GraphDB.Storage as S
import qualified GraphDB.Persistence.TransactionLog as TL


------------------
-- DB
------------------

-- |
-- A persistence wrapper backend for any other serializable backend,
-- e.g., an in-memory graph.
data Persistence b = Persistence !b !(S.Storage b (TL.Log b)) !IOQueue.IOQueue

-- |
-- A constraint for any serializable backend implementation.
type SerializableBackend b =
  (B.Backend b, Serializable IO b, 
   Serializable IO (B.Value b), Serializable IO (B.Type b), Serializable IO (B.Index b))

start :: (SerializableBackend b) => Settings b -> IO (Persistence b)
start (bufferSize, paths, initBackend) = do
  (storage, backend) <- S.acquireAndLoad initBackend TL.apply paths
  buffer <- IOQueue.start bufferSize
  return $ Persistence backend storage buffer

stop :: (Serializable IO b) => Persistence b -> IO ()
stop (Persistence g s b) = do
  IOQueue.shutdown b
  S.checkpoint s g
  S.release s

-- |
-- Run a computation on Persistence, 
-- while automatically acquiring and releasing all related resources.
with :: SerializableBackend b => Settings b -> (Persistence b -> IO r) -> IO r
with settings = bracket (start settings) stop

------------------
-- Settings
------------------

-- |
-- Persistence settings.
type Settings b = (BufferSize, S.Paths, InitBackend b)
-- |
-- An admissible amount of transactions,
-- by which the persistence layer may be lagging behind the actual state of the graph. 
-- Until that amount is reached the persistence of transactions is done asynchronously,
-- thus reducing the time of their execution and of acquisition of related locks.
-- If you want the persisted state to always accomodate to the actual in-memory state,
-- set this to @1@. 
-- Thus you can make sure that the persistence of events is always done synchronously.
type BufferSize = Int
-- |
-- A default initializer for backend.
-- Will only be used if the graph hasn't been previously persisted,
-- i.e. on the first run of the DB.
type InitBackend b = IO b
-- | 
-- Determine paths from a unique name among all storages running on this machine. 
-- It will be used to set default values for storage paths under \"~\/.graph-db\/\[name\]\/\".
pathsFromName :: Text -> IO S.Paths
pathsFromName name = S.pathsFromDirectory ("~/.graph-db/" <> FS.fromText name)

------------------
-- Transactions
------------------

instance (B.Backend b, Serializable IO (TL.Log b)) => B.Backend (Persistence b) where
  type Union (Persistence b) = B.Union b
  newtype Tx (Persistence b) r = Tx (RWST () [TL.Entry b] Int (B.Tx b) r)
  type Node (Persistence b) = (Int, (B.Node b))
  runRead (Tx tx) (Persistence g _ _) = do
    (r, _) <- B.runRead (evalRWST tx () 0) g
    return r
  runWrite (Tx tx) (Persistence g s b) = do
    (r, entries) <- B.runWrite (evalRWST tx () 0) g
    IOQueue.enqueue b $ S.persistEvent s $ TL.Log entries
    return r
  newNode v = do
    Tx $ tell $ pure $ TL.NewNode v
    newBaseNodeTx =<< do Tx $ lift $ B.newNode v
  getValue (_, n) = do
    B.getValue n |> lift |> Tx
  setValue (i, n) v = do
    TL.SetValue i v |> pure |> tell |> Tx
    B.setValue n v |> lift |> Tx
  getRoot = do
    Tx $ tell $ pure $ TL.GetRoot
    newBaseNodeTx =<< do Tx $ lift $ B.getRoot
  getTargetsByType (i, n) t = do
    TL.GetTargetsByType i t |> pure |> tell |> Tx
    B.getTargetsByType n t |> lift |> Tx >>= mapM newBaseNodeTx
  getTargetsByIndex (si, sn) i = do
    TL.GetTargetsByIndex si i |> pure |> tell |> Tx
    B.getTargetsByIndex sn i |> lift |> Tx >>= mapM newBaseNodeTx
  addTarget (si, sn) (ti, tn) = do
    Tx $ tell $ pure $ TL.AddTarget si ti
    Tx $ lift $ B.addTarget sn tn
  removeTarget (si, sn) (ti, tn) = do
    Tx $ tell $ pure $ TL.RemoveTarget si ti
    Tx $ lift $ B.removeTarget sn tn
  getStats (i, n) = do
    Tx $ lift $ B.getStats n

instance MonadIO (B.Tx b) => MonadIO (B.Tx (Persistence b)) where
  liftIO = Tx . liftIO

instance Monad (B.Tx b) => Monad (B.Tx (Persistence b)) where
  return = Tx . return
  Tx a >>= k = Tx $ a >>= return . k >>= \(Tx b) -> b

-- Why it won't compile without a Monad constraint is a mystery.
instance (Applicative (B.Tx b), Monad (B.Tx b)) => Applicative (B.Tx (Persistence b)) where 
  pure = Tx . pure
  Tx a <*> Tx b = Tx $ a <*> b

instance Functor (B.Tx b) => Functor (B.Tx (Persistence b)) where
  fmap f (Tx a) = Tx $ fmap f a

newBaseNodeTx :: Monad (B.Tx b) => B.Node b -> B.Tx (Persistence b) (B.Node (Persistence b))
newBaseNodeTx n = Tx $ do
  index <- get
  modify succ
  return $ (index, n) 

