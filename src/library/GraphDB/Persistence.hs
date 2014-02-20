{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence
  (
    -- * DB
    Persistence,
    with,
    -- ** Settings
    Settings,
    BufferSize,
    Paths,
    InitBackend,
    pathsFromName,
    S.pathsFromDirectory,
  )
  where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as IOQueue
import qualified GraphDB.Transaction.Backend as B
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
type Settings b = (BufferSize, Paths, InitBackend b)
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
-- Storage paths. Determines where to store logs, checkpoints and archive.
type Paths = S.Paths
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

type Tx b = RWST () [TL.Entry b] Int (B.Tx b)

instance (B.Backend b, Serializable IO (TL.Log b)) => 
         B.Backend (Persistence b) where
  type Tx (Persistence b) = Tx b
  data Node (Persistence b) = Node Int (B.Node b)
  newtype Value (Persistence b) = Value (B.Value b)
  newtype Type (Persistence b) = Type (B.Type b)
  newtype Index (Persistence b) = Index (B.Index b)
  runRead tx (Persistence g _ _) = do
    (r, _) <- B.runRead (evalRWST tx () 0) g
    return r
  runWrite tx (Persistence g s b) = do
    (r, entries) <- B.runWrite (evalRWST tx () 0) g
    IOQueue.enqueue b $ S.persistEvent s $ TL.Log entries
    return r
  newNode (Value v) = do
    tell $ pure $ TL.NewNode v
    newGraphNodeTx =<< do lift $ B.newNode v
  getValue (Node _ n) = do
    B.getValue n |> lift |> fmap Value
  setValue (Node i n) (Value v) = do
    TL.SetValue i v |> pure |> tell
    B.setValue n v |> lift
  getRoot = do
    tell $ pure $ TL.GetRoot
    newGraphNodeTx =<< do lift $ B.getRoot
  getTargetsByType (Node i n) (Type t) = do
    TL.GetTargetsByType i t |> pure |> tell
    B.getTargetsByType n t |> lift >>= mapM newGraphNodeTx
  getTargetsByIndex (Node si sn) (Index i) = do
    TL.GetTargetsByIndex si i |> pure |> tell
    B.getTargetsByIndex sn i |> lift >>= mapM newGraphNodeTx
  addTarget (Node si sn) (Node ti tn) = do
    tell $ pure $ TL.AddTarget si ti
    lift $ B.addTarget sn tn
  removeTarget (Node si sn) (Node ti tn) = do
    tell $ pure $ TL.RemoveTarget si ti
    lift $ B.removeTarget sn tn
  getStats (Node i n) = do
    lift $ B.getStats n

newGraphNodeTx :: Monad (B.Tx b) => B.Node b -> Tx b (B.Node (Persistence b))
newGraphNodeTx n = do
  index <- get
  modify succ
  return $ Node index n

