-- |
-- The API is based on several layers of monads and monad transformers:
-- 
-- * 'Session'. 
-- The main monad transformer.
-- It executes transactions and runs the server.
-- 
-- * 'Read' and 'Write' transactions.
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
module GraphDB where

import GraphDB.Util.Prelude hiding (write, read, Write, Read)



-- * Session
-------------------------

-- |
-- A monad transformer, 
-- which can execute transactions and run a server over some @backend@.
-- The specifics of the implementation are provided by the instance of 
-- the 'Backend' class.
data Session backend union (monad :: * -> *) result

-- |
-- Session failure.
data SessionFailure b =
  BackendFailure (BackendFailure b)

-- |
-- Execute a writing transaction.
-- 
-- Does not allow concurrent transactions, 
-- so all concurrent transactions are put on hold for the time of execution.
write :: (forall s. Write u s r) -> Session b u m r
write = $notImplemented

-- |
-- Execute a read-only transaction.
-- Gets executed concurrently.
read :: (forall s. Read u s r) -> Session b u m r
read = $notImplemented

-- |
-- Run a server on this session.
serve :: Serve b u m r -> Session b u m r
serve = $notImplemented

-- |
-- Run a session on a backend with the provided settings.
runSession :: (Backend b) => BackendSettings b -> Session b u m r -> m (Either (SessionFailure b) r)
runSession = $notImplemented



-- * Backends
-------------------------

-- |
-- A session backend.
class Backend b where
  -- |
  -- A failure inherent to the backend.
  data BackendFailure b
  -- |
  -- Settings for running a session on the backend.
  data BackendSettings b



-- ** Graph
-------------------------

-- |
-- An in-memory graph datastructure.
data Graph

instance Backend Graph where
  data BackendFailure Graph
  data BackendSettings Graph



-- ** Client
-------------------------

-- | A networking interface for communication with server.
data Client

instance Backend Client where
  data BackendFailure Client
  data BackendSettings Client



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

instance (Backend w) => Backend (Persistence w) where
  data BackendFailure (Persistence w)
  data BackendSettings (Persistence w)



-- * Transactions
-------------------------

-- | 
-- A write and read transaction.
data Write union stateThread result

-- | 
-- A read-only transaction. 
data Read union stateThread result



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

