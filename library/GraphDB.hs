-- |
-- The API is based on several layers of monads and monad transformers:
-- 
-- * 'Session'. 
-- The main monad transformer.
-- It executes transactions and runs the server.
-- 
-- * 'Transaction.Read' and 'Transaction.Write' transactions.
-- Monads,
-- which execute granular updates or reads on the database with ACID guarantees.
-- 
-- * 'Serve'.
-- A monad transformer for running a server.
-- It can only be executed inside a session,
-- thus serving the backend of that session.
-- 
-- Sessions are parameterized by types with an instance of the 'Backend' class. 
-- Essentially, backend is an implementation of the database engine.
-- The library provides three types of those:
-- 
-- * An in-memory 'Graph' data structure.
-- 
-- * A 'Persistence' wrapper for any backend. 
-- 
-- * A 'Client'. The networking interface for communication with server.
-- 
-- The API of this library is free of exceptions and resource management.
-- This is achieved using monad transformers.
-- All the IO failures are encoded in the results of monad transformers.
-- All the resources are properly released.
module GraphDB
(
  -- * Session
  Backend.Backend,
  Backend.Session,
  Backend.SessionSettings,
  Backend.SessionResult,
  Backend.runSession,
  write,
  read,
  serve,
  -- ** Backends
  Graph,
  Client,
  Persistence,
  -- * Transactions
  Transaction.Read,
  Transaction.Write,
  Transaction.ReadOrWrite,
  Transaction.Node,
  -- ** Operations
  Transaction.newNode,
  Transaction.getValue,
  Transaction.setValue,
  Transaction.getRoot,
  Transaction.getTargetsByType,
  Transaction.getTargetsByIndex,
  Transaction.addTarget,
  Transaction.removeTarget,
  Transaction.getStats,
  -- * Modeling
  Union.Union,
  Union.PolyValue,
  Union.PolyIndex,
  Edge.Edge(..),
  Macros.generateUnion,
  -- * Server
  Serve,
  block,
)
where

import GraphDB.Util.Prelude hiding (write, read, Write, Read, block)
import qualified GraphDB.Backend as Backend
import qualified GraphDB.FreeTransaction as Transaction
import qualified GraphDB.FreeTransaction.Action as Action
import qualified GraphDB.Model.Union as Union
import qualified GraphDB.Model.Edge as Edge
import qualified GraphDB.Model.Macros as Macros


-- * Session
-------------------------

-- |
-- Execute a writing transaction.
-- 
-- Does not allow concurrent transactions, 
-- so all concurrent transactions are put on hold for the time of execution.
write :: (Backend.Backend b, Monad m) => (forall s. Transaction.Write b u s r) -> Backend.Session b u m r
write (Transaction.Write a) = Backend.runAction True a

-- |
-- Execute a read-only transaction.
-- Gets executed concurrently.
read :: (Backend.Backend b, Monad m) => (forall s. Transaction.Read b u s r) -> Backend.Session b u m r
read (Transaction.Read a) = Backend.runAction False a

-- |
-- Run a server on this session.
serve :: Serve b u m r -> Backend.Session b u m r
serve = $notImplemented



-- * Backends
-------------------------



-- ** Graph
-------------------------

-- |
-- An in-memory graph datastructure.
data Graph



-- ** Client
-------------------------

-- | A networking interface for communication with server.
data Client



-- ** Persistence
-------------------------

-- |
-- A wrapper over another backend, 
-- which adds a persistence functionality on top.
-- 
-- Being a backend itself,
-- it allows one to construct stacks of backends. E.g.:
-- 
-- * @Persistence 'Client'@ - 
-- adds a safety layer over a networking interface.
-- 
-- * @Persistence (Persistence anotherBackend)@ -
-- persists the updates to multiple locations on filesystem for safety.
-- 
-- * @Persistence 'Graph'@ -
-- not hard to guess what it does.
data Persistence wrappedBackend



-- * Server
-------------------------

-- |
-- A monad transformer for running the server.
-- 
-- Can only be executed inside a backend 'Session' using 'serve',
-- thus serving that particular backend.
data Serve backend union (monad :: * -> *) result

-- |
-- Block the calling thread until the server stops (which should never happen).
block :: Serve b u m ()
block = $notImplemented

