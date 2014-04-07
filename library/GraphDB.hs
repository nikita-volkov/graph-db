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
-- thus serving the engine of that session.
-- 
-- Sessions are parameterized by types with an instance of the 'Engine' class. 
-- Essentially those are what defines the behaviour of session.
-- The library provides three types of those:
-- 
-- * An in-memory 'Nonpersistent' data structure.
-- 
-- * A 'Persistent' version of an in-memory data structure. 
-- 
-- * A 'Client', the networking interface for communication with server.
-- 
-- The API of this library is free of exceptions and resource management.
-- This is achieved using monad transformers.
-- All the IO failures are encoded in the results of monad transformers.
-- All the resources are properly released.
module GraphDB
(
  -- * Session
  Session,
  read,
  write,
  Engine,
  -- * Engines
  -- ** Nonpersistent
  Nonpersistent,
  runNonpersistentSession,
  -- ** Persistent
  Persistent,
  PersistentSettings,
  Persistence.StoragePath,
  Persistence.PersistenceBuffering,
  Persistence.PersistenceFailure(..),
  runPersistentSession,
  -- ** Client
  Client,
  ClientSettings,
  RemotionClient.UserProtocolSignature,
  RemotionClient.URL(..),
  RemotionClient.Credentials,
  RemotionClient.Failure(..),
  runClientSession,
  -- * Transactions
  Read,
  Write,
  ReadOrWrite,
  Node,
  -- ** Operations
  newNode,
  getValue,
  setValue,
  getRoot,
  getTargetsByType,
  getTargetsByIndex,
  addTarget,
  removeTarget,
  getStats,
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
import qualified GraphDB.Model.Union as Union
import qualified GraphDB.Model.Edge as Edge
import qualified GraphDB.Model.Macros as Macros
import qualified GraphDB.Action as Action
import qualified GraphDB.Graph as Graph
import qualified GraphDB.Graph.Node as Node
import qualified GraphDB.Client as Client
import qualified GraphDB.Persistence as Persistence
import qualified Remotion.Client as RemotionClient



-- * Session
-------------------------

-- |
-- A monad transformer, 
-- which can execute transactions and run a server over some engine.
newtype Session e u m r = Session (EngineSession e u m r)

type Action e u = 
  Action.Action (EngineNode e u) (Union.Value u) (Union.Type u) (Union.Index u) Identity

-- |
-- A session engine.
class Engine e where
  type EngineSession e u m r
  type EngineNode e u
  runTransaction :: 
    (Union.Union u, MonadBaseControl IO m, MonadIO m) => 
    Bool -> Action e u r -> Session e u m r

-- |
-- Execute a writing transaction.
-- 
-- Does not allow concurrent transactions, 
-- so all concurrent transactions are put on hold for the time of execution.
write :: 
  (Engine e, Union.Union u, MonadBaseControl IO m, MonadIO m) => 
  (forall s. Write e u s r) -> Session e u m r
write (Write a) = runTransaction True a

-- |
-- Execute a read-only transaction.
-- Gets executed concurrently.
read :: 
  (Engine e, Union.Union u, MonadBaseControl IO m, MonadIO m) => 
  (forall s. Read e u s r) -> Session e u m r
read (Read a) = runTransaction False a

-- -- |
-- -- Run a server on this session.
-- serve :: Serve e u m r -> Session e u m r
-- serve = $notImplemented



-- * Engines
-------------------------



-- ** Nonpersistent
-------------------------

-- |
-- An in-memory graph datastructure with no persistence.
data Nonpersistent

instance Engine Nonpersistent where
  type EngineSession Nonpersistent u m r = Graph.Session u m r
  type EngineNode Nonpersistent u = Graph.Node u
  runTransaction w a = 
    Session $ Graph.runTransaction w $ Graph.runAction $ 
    hoistFreeT (return . runIdentity) $ a

-- |
-- Run a nonpersistent session, 
-- while providing an initial value for the root node.
runNonpersistentSession :: (Union.PolyValue u v, MonadIO m) => v -> Session Nonpersistent u m r -> m r
runNonpersistentSession v (Session s) = do
  n <- liftIO $ Node.new $ snd $ Union.packValue $ v
  Graph.runSession n s



-- ** Persistent
-------------------------

-- |
-- An in-memory graph datastructure with persistence.
data Persistent

instance Engine Persistent where
  type EngineSession Persistent u m r = Persistence.Session u m r
  type EngineNode Persistent u = Int
  runTransaction w a =
    Session $ Persistence.runTransaction w $ Persistence.runAction $ 
    hoistFreeT (return . runIdentity) $ a

-- |
-- Settings of a persistent session.
-- 
-- The first parameter is an initial value for the root node.
-- It will only be used if the graph has not been previously persisted,
-- i.e. on the first run of the DB.
type PersistentSettings v = (v, Persistence.StoragePath, Persistence.PersistenceBuffering)

-- |
-- Run a persisted session with settings.
runPersistentSession :: 
  (MonadIO m, MonadBaseControl IO m, Union.PolyValue u v) => 
  PersistentSettings v -> Session Persistent u m r -> m (Either Persistence.PersistenceFailure r)
runPersistentSession (v, p, e) (Session s) = do
  Persistence.runSession (snd $ Union.packValue $ v, p, e) s



-- ** Client
-------------------------

-- | 
-- A networking interface for communication with server.
data Client

instance Engine Client where
  type EngineSession Client u m r = Client.Session u m r
  type EngineNode Client u = Client.Node
  runTransaction w a =
    Session $ Client.runTransaction w $ Client.runAction $ 
    hoistFreeT (return . runIdentity) $ a

-- | 
-- Settings of a client session.
type ClientSettings = (RemotionClient.UserProtocolSignature, RemotionClient.URL)

-- |
-- Run a client session with settings.
runClientSession :: 
  (MonadIO m, MonadBaseControl IO m, Union.Union u) =>
  ClientSettings -> Session Client u m r -> m (Either RemotionClient.Failure r)
runClientSession s (Session ses) = Client.runSession s ses



-- * Transactions
-------------------------

-- | 
-- A read-only transaction. 
-- 
-- Gets executed concurrently.
newtype Read e u s r = 
  Read (Action e u r) 
  deriving (Functor, Applicative, Monad)

-- | 
-- A write and read transaction.
-- 
-- Does not allow concurrency, 
-- so all concurrent transactions are put on hold for the time of its execution.
newtype Write e u s r = 
  Write (Action e u r) 
  deriving (Functor, Applicative, Monad)

-- |
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite e u s r = 
  forall t. (LiftAction t, Monad (t e u s), Applicative (t e u s)) => 
  t e u s r

class LiftAction t where 
  liftAction :: Action e u r -> t e u s r
instance LiftAction Read where liftAction = Read
instance LiftAction Write where liftAction = Write

-- | 
-- A transaction-local reference to an actual node of the graph.
-- 
-- @s@ is a so called "state thread".
-- It is an uninstantiated type-variable,
-- which makes it impossible to return a node from transaction,
-- when it is executed using 'write' or 'read'.
-- Much inspired by the implementation of 'ST'.
newtype Node e u s v = Node (EngineNode e u)

-- |
-- Create a new node. 
-- 
-- This node won't get stored if you don't insert at least a single edge 
-- from another stored node to it.
newNode :: (Union.PolyValue u v) => v -> Write e u s (Node e u s v)
newNode v = fmap Node $ liftAction $ Action.newNode $ snd $ Union.packValue v

-- | 
-- Get a value of the node.
getValue :: (Union.PolyValue u v) => Node e u s v -> ReadOrWrite e u s v
getValue (Node n) = 
  fmap (fromMaybe ($bug "Unexpected packed value") . Union.unpackValue) $ 
  liftAction $ Action.getValue n

-- | 
-- Replace the value of the specified node.
setValue :: (Union.PolyValue u v) => Node e u s v -> v -> Write e u s ()
setValue (Node n) v = Write $ Action.setValue n (snd $ Union.packValue v)

-- |
-- Get the root node.
getRoot :: ReadOrWrite e u s (Node e u s u)
getRoot = fmap Node $ liftAction $ Action.getRoot

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType node (undefined :: Artist)
-- 
getTargetsByType :: (Union.PolyValue u v') => Node e u s v -> v' -> ReadOrWrite e u s [Node e u s v']
getTargetsByType (Node n) v =
  fmap (map Node) $ liftAction $ Action.getTargetsByType n $ fst $ Union.packValue v

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: (Union.PolyIndex u i) => Node e u s v -> i -> ReadOrWrite e u s [Node e u s v']
getTargetsByIndex (Node n) i = 
  fmap (map Node) $ liftAction $ Action.getTargetsByIndex n $ Union.packIndex i

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is already there it will return 'False'.
addTarget :: (Edge.Edge v v') => Node e u s v -> Node e u s v' -> Write e u s Bool
addTarget (Node s) (Node t) = Write $ Action.addTarget s t

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node is not found it will return 'False'.
removeTarget :: (Edge.Edge v v') => Node e u s v -> Node e u s v' -> Write e u s Bool
removeTarget (Node s) (Node t) = Write $ Action.removeTarget s t

-- |
-- Count the total amounts of distinct nodes and edges in the graph.
-- 
-- Requires a traversal of the whole graph, so beware.
getStats :: ReadOrWrite e u s (Int, Int)
getStats = liftAction $ Action.getStats




-- * Server
-------------------------

-- |
-- A monad transformer for running the server.
-- 
-- Can only be executed inside a engine 'Session' using 'serve',
-- thus serving that particular engine.
data Serve engine union (monad :: * -> *) result

-- |
-- Block the calling thread until the server stops (which should never happen).
block :: Serve e u m ()
block = $notImplemented

