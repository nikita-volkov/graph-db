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
    DefaultRoot,
    pathsFromName,
  )
  where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as IOQueue
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Union as U
import qualified GraphDB.Storage as S
import qualified GraphDB.Persistence.TransactionLog as TL
import qualified GraphDB.Graph as G


------------------
-- DB
------------------

-- |
-- A local database with persistence.
data Persistence u = Persistence !(G.Graph u) !(Storage u) !IOQueue.IOQueue
type Storage u = S.Storage (G.Graph u) (TL.Log u)

start :: forall u a. (U.Union u, U.PolyValue u a) => Settings a -> IO (Persistence u)
start (bufferSize, paths, defaultRoot) = do
  (storage, graph) <- S.acquireAndLoad (G.new defaultRoot) TL.apply paths
  buffer <- IOQueue.start bufferSize
  return $ Persistence graph storage buffer

stop :: (U.Union u) => Persistence u -> IO ()
stop (Persistence g s b) = do
  IOQueue.shutdown b
  S.checkpoint s $notImplemented
  S.release s

with :: (U.Union u, U.PolyValue u a) => Settings a -> (Persistence u -> IO r) -> IO r
with settings = bracket (start settings) stop

------------------
-- Settings
------------------

-- |
-- Persistence settings.
type Settings a = (BufferSize, Paths, DefaultRoot a)
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
-- A default value for root node. 
-- Will only be used if the graph hasn't been previously persisted,
-- i.e. on the first run of the DB.
type DefaultRoot a = a
-- | 
-- Determine paths from a unique name among all storages running on this machine. 
-- It will be used to set default values for storage paths under \"~\/.graph-db\/\[name\]\/\".
pathsFromName :: Text -> IO S.Paths
pathsFromName name = S.pathsFromDirectory ("~/.graph-db/" <> FS.fromText name)

------------------
-- Transactions
------------------

type Tx u = RWST () [TL.Entry u] Int (B.Tx (G.Graph u))

instance (U.Union u) => B.Backend (Persistence u) where
  type Tx (Persistence u) = Tx u
  data Node (Persistence u) = Node Int (B.Node (G.Graph u))
  type Value (Persistence u) = U.Value u
  type Type (Persistence u) = U.Type u
  type Index (Persistence u) = U.Index u
  runRead tx (Persistence g _ _) = do
    (r, _) <- B.runRead (evalRWST tx () 0) g
    return r
  runWrite tx (Persistence g s b) = do
    (r, entries) <- B.runWrite (evalRWST tx () 0) g
    IOQueue.enqueue b $ S.persistEvent s $ TL.Log entries
    return r
  getRoot = do
    tell $ pure $ TL.GetRoot
    newGraphNodeTx =<< do lift $ B.getRoot
  addTarget (Node si sn) (Node ti tn) = do
    tell $ pure $ TL.AddTarget si ti
    lift $ B.addTarget sn tn

newGraphNodeTx :: B.Node (G.Graph u) -> Tx u (B.Node (Persistence u))
newGraphNodeTx n = do
  index <- get
  modify succ
  return $ Node index n

