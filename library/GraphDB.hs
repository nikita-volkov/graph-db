{-# LANGUAGE UndecidableInstances #-}
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
  ClientModelVersion,
  RemotionClient.URL(..),
  RemotionClient.Credentials,
  ClientFailure(..),
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
  ServerSettings,
  ServerModelVersion,
  RemotionServer.ListeningMode,
  RemotionServer.Timeout,
  RemotionServer.MaxClients,
  RemotionServer.Log,
  ServerFailure(..),
  serve,
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
import qualified GraphDB.Server as Server
import qualified Remotion.Client as RemotionClient
import qualified Remotion.Server as RemotionServer



-- * Session
-------------------------

-- |
-- A monad transformer, 
-- which can execute transactions and run a server using some engine.
newtype Session e u m r = 
  Session ((Monad (EngineSession e u m)) => EngineSession e u m r)

instance Monad (Session e u m) where
instance Functor (Session e u m)
instance Applicative (Session e u m)
instance MonadTrans (Session e u)
instance MonadIO (Session e u m)
instance MonadBase IO (Session e u m)
instance MonadBaseControl IO (Session e u m)


type Action e u = 
  Action.Action (EngineNode e u) (Union.Value u) (Union.Type u) (Union.Index u)

-- |
-- A session engine.
class Engine e where
  type EngineSession e u m
  type EngineNode e u
  runTransaction :: 
    (Union.Union u, MonadBaseControl IO m, MonadIO m) => 
    Bool -> Action e u m r -> Session e u m r

-- |
-- Execute a writing transaction.
-- 
-- Does not allow concurrent transactions, 
-- so all concurrent transactions are put on hold for the time of execution.
-- 
-- Concerning the \"forall\" part refer to 'Node'.
write :: 
  (Engine e, Union.Union u, MonadBaseControl IO m, MonadIO m) => 
  (forall s. Write e u s r) -> Session e u m r
write (Write a) = runTransaction True $ hoistFreeT (return . runIdentity) $ a

-- |
-- Execute a read-only transaction.
-- Gets executed concurrently.
-- 
-- Concerning the \"forall\" part refer to 'Node'.
read :: 
  (Engine e, Union.Union u, MonadBaseControl IO m, MonadIO m) => 
  (forall s. Read e u s r) -> Session e u m r
read (Read a) = runTransaction False $ hoistFreeT (return . runIdentity) $ a



-- * Engines
-------------------------



-- ** Nonpersistent
-------------------------

-- |
-- An in-memory graph datastructure with no persistence.
data Nonpersistent

instance Engine Nonpersistent where
  type EngineSession Nonpersistent u m = Graph.Session u m
  type EngineNode Nonpersistent u = Graph.Node u
  runTransaction w a = 
    Session $ Graph.runTransaction w $ Graph.runAction $ a

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
  type EngineSession Persistent u m = Persistence.Session u m
  type EngineNode Persistent u = Int
  runTransaction w a =
    Session $ Persistence.runTransaction w $ Persistence.runAction $ a

-- |
-- Settings of a persistent session.
-- 
-- The first parameter is an initial value for the root node.
-- It will only be used if the graph has not been previously persisted,
-- i.e. on the first run of the DB.
type PersistentSettings v = (v, Persistence.StoragePath, Persistence.PersistenceBuffering)

-- |
-- Run a persistent session with settings.
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
  type EngineSession Client u m = Client.Session u m
  type EngineNode Client u = Int
  runTransaction w a =
    Session $ Client.runTransaction w $ Client.runAction $ a

-- | 
-- Settings of a client session.
type ClientSettings = (ClientModelVersion, RemotionClient.URL)

-- |
-- Version of the graph model, 
-- which is used to check the client and server compatibility during handshake.
type ClientModelVersion = Int

data ClientFailure =
  -- |
  -- Unable to connect to the provided url.
  UnreachableURL |
  -- |
  -- The server has too many connections already.
  -- It's suggested to retry later.
  ServerIsBusy |
  -- |
  -- Incorrect credentials.
  Unauthenticated |
  -- |
  -- Either the connection got interrupted for some reason or
  -- a communication timeout has been reached.
  ConnectionFailure |
  -- | 
  -- Either the graph model does not match the one on the server or
  -- the server runs an incompatible version of \"graph-db\".
  Incompatible |
  -- | 
  -- The server was unable to deserialize the request.
  -- This is only expected to happen when the same 'ClientModelVersion' 
  -- was used for incompatible models.
  CorruptRequest Text
  deriving (Show, Eq)

-- |
-- Run a client session with settings.
runClientSession :: 
  (MonadIO m, MonadBaseControl IO m, Union.Union u) =>
  ClientSettings -> Session Client u m r -> m (Either ClientFailure r)
runClientSession (v, url) (Session ses) = 
  fmap (fmapL adaptRemotionFailure) $ Client.runSession (rv, url) $ ses
  where
    adaptRemotionFailure = \case
      RemotionClient.UnreachableURL -> UnreachableURL
      RemotionClient.ServerIsBusy -> ServerIsBusy
      RemotionClient.ProtocolVersionMismatch _ _ -> Incompatible
      RemotionClient.UserProtocolSignatureMismatch _ _ -> Incompatible
      RemotionClient.Unauthenticated -> Unauthenticated
      RemotionClient.ConnectionInterrupted -> ConnectionFailure
      RemotionClient.TimeoutReached _ -> ConnectionFailure
      RemotionClient.CorruptRequest t -> CorruptRequest t
    rv = fromString $ show $ v



-- * Transactions
-------------------------

-- | 
-- A read-only transaction. 
-- 
-- Gets executed concurrently.
newtype Read e u s r = 
  Read (Action e u Identity r)
  deriving (Functor, Applicative, Monad)

-- | 
-- A write and read transaction.
-- 
-- Does not allow concurrency, 
-- so all concurrent transactions are put on hold for the time of its execution.
newtype Write e u s r = 
  Write (Action e u Identity r)
  deriving (Functor, Applicative, Monad)

-- |
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite e u s r = 
  forall t. (LiftAction t, Monad (t e u s), Applicative (t e u s)) => 
  t e u s r

class LiftAction t where 
  liftAction :: Action e u Identity r -> t e u s r
instance LiftAction Read where liftAction = Read
instance LiftAction Write where liftAction = Write

-- | 
-- A transaction-local reference to an actual node of the graph.
-- 
-- @s@ is the so called \"state thread\".
-- It is an uninstantiated type-variable,
-- which makes it impossible to return a node from transaction,
-- when it is executed using 'write' or 'read'.
-- Much inspired by the implementation of 'ST'.
newtype Node e u s v = Node (EngineNode e u)



-- ** Operations
-------------------------

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
-- Settings of server.
type ServerSettings u = 
  (
    ServerModelVersion, 
    RemotionServer.ListeningMode, 
    RemotionServer.Timeout,
    RemotionServer.MaxClients,
    RemotionServer.Log
  )

-- |
-- Version of the graph model, 
-- which is used to check the client and server compatibility during handshake.
type ServerModelVersion = Int

-- | 
-- A server failure.
data ServerFailure =
  ListeningSocketIsBusy

-- |
-- Run a server on this session.
serve :: 
  (MonadIO m, MonadBaseControl IO m, Engine e, Union.Union u) => 
  ServerSettings u -> Session e u m (Either ServerFailure r)
serve (v, lm, to, mc, log) = do
  transactionsChan <- liftIO $ newChan
  let
    ups = fromString $ show $ v
    pur = Server.processRequest transactionsChan
    settings = (ups, lm, to, mc, log, pur)
  r <- RemotionServer.run settings $ liftWith $ \restore -> do
    forever $ do
      (w, comm) <- liftIO $ readChan transactionsChan
      async $ runTransaction w $ Server.runCommandProcessor comm
  return $ fmapL adaptRemotionFailure $ r
  where
    adaptRemotionFailure = \case
      RemotionServer.ListeningSocketIsBusy -> ListeningSocketIsBusy


