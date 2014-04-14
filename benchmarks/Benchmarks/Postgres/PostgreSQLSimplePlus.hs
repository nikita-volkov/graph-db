-- |
-- A simpler wrapper API for "postgresql-simple" and "ex-pool".
module Benchmarks.Postgres.PostgreSQLSimplePlus 
(
  -- * Session
  Session,
  runSession,
  -- ** Settings
  Settings,
  Host,
  Port,
  User,
  Password,
  Database,
  PoolSize,
  -- * Action
  Action,
  runAction,
  query,
  query_,
  execute,
  execute_,
  returning,
  -- * Reexports of from "postgresql-simple"
  Pos.ToRow(..),
  Pos.FromRow(..),
  Pos.ToField(..),
  Pos.FromField(..),
  Pos.field,
  Pos.Only(..),
  Pos.fromOnly,
  Pos.sql,
)
where

import Benchmarks.Prelude
import qualified Database.PostgreSQL.Simple as Pos
import qualified Database.PostgreSQL.Simple.SqlQQ as Pos
import qualified Database.PostgreSQL.Simple.FromField as Pos
import qualified Database.PostgreSQL.Simple.ToField as Pos
import qualified Database.PostgreSQL.Simple.FromRow as Pos
import qualified Database.PostgreSQL.Simple.ToRow as Pos
import qualified Data.Pool as Poo


-- * Session
-------------------------

newtype Session m r = Session (ReaderT (Poo.Pool Pos.Connection) m r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runSession :: MonadIO m => Settings -> Session m r -> m r
runSession (host, port, user, pw, db, poolSize) (Session m) = do
  pool <- liftIO $ Poo.createPool connect disconnect stripe timeout poolSize
  r <- runReaderT m pool
  liftIO $ Poo.purgePool pool
  return r
  where
    connect = Pos.connect $ Pos.ConnectInfo host port user pw db
    disconnect c = Pos.close c
    stripe = 1
    timeout = fromIntegral 30

type Settings = (Host, Port, User, Password, Database, PoolSize)
type Host = String
type Port = Word16
type User = String
type Password = String
type Database = String
type PoolSize = Word32


-- * Action
-------------------------

-- |
-- A composition of multiple queries, 
-- which will be executed on a single connection and
-- optionally as a single transaction.
-- 
-- 'MonadIO' instance is not provided intentionally,
-- since transactions must not produce any side effects besides
-- the database updates.
newtype Action r = Action (ReaderT Pos.Connection IO r)
  deriving (Functor, Applicative, Monad)

-- |
-- Whether to treat a composed action as a transaction.
type AsTransaction = Bool

-- FIXME: handle exceptions and rollbacks by retrying, 
-- if this code is to be used anywhere seriously.
runAction :: (MonadIO m) => AsTransaction -> Action r -> Session m r
runAction asTransaction (Action m) = do
  pool <- Session ask
  liftIO $ Poo.withResource pool $ \cx -> 
    (if asTransaction then Pos.withTransaction cx else id) $ 
      runReaderT m cx

-- ** Actions
-------------------------

query :: (Pos.ToRow a, Pos.FromRow r) => Pos.Query -> a -> Action [r]
query q a = Action $ ReaderT $ \c -> Pos.query c q a

query_ :: (Pos.FromRow r) => Pos.Query -> Action [r]
query_ q = Action $ ReaderT $ \c -> Pos.query_ c q

execute :: (Pos.ToRow a) => Pos.Query -> a -> Action ()
execute q a = Action $ ReaderT $ \c -> void $ Pos.execute c q a

execute_ :: Pos.Query -> Action ()
execute_ q = Action $ ReaderT $ \c -> void $ Pos.execute_ c q

returning :: (Pos.ToRow a, Pos.FromRow r) => Pos.Query -> [a] -> Action [r]
returning q a = Action $ ReaderT $ \c -> Pos.returning c q a
