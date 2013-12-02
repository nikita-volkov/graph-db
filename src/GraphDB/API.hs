module GraphDB.API
  (
    -- * DB Engine
    Engine,
    startEngine,
    shutdownEngine,
    shutdownEngine',
    runEvent,
    Mode(..),
    pathsFromName,
    Storage.pathsFromDirectory,
    Storage.Paths,
    URL(..),

    -- * Transactions
    Graph.Write,
    Graph.Read,
    Graph.ReadOrWrite,
    Graph.Node,
    Graph.Reachable(..),

    -- ** Transaction building blocks
    Graph.getRoot,
    Graph.newNode,
    Graph.getTargetsByType,
    Graph.getTargetsByIndex,
    Graph.addTarget,
    Graph.removeTarget,
    Graph.getValue,
    Graph.setValue,
    Graph.countTargets,
    Graph.getStats,

    -- * Server
    ServerMode(..),
    Server,
    startServer,
    shutdownServer,

    -- * Boilerplate
    GraphDBTag(..),
    Graph.GraphTag(..),
    Transaction(..),
    Graph.IsUnionValue(..),
    IsUnionEvent(..),
    IsUnionEventResult(..),
  ) 
  where

import GraphDB.Prelude hiding (Read, log)
import qualified GraphDB.IOQueue as IOQueue; import GraphDB.IOQueue (IOQueue)
import qualified GraphDB.Graph as Graph
import qualified AcidIO.Storage as Storage
import qualified AcidIO.Server as Server
import qualified AcidIO.Client as Client
import qualified Filesystem.Path.CurrentOS as FilePath


-- | 
-- Engine running mode.
data Mode =
  -- | 
  -- Run in current process. 
  -- If no paths are provided, then there'll be no persistence.
  -- 
  -- [@Usage@]
  -- 
  -- > Mode_Local (Just (eventsPersistenceBufferSize, persistencePaths))
  -- 
  -- [@eventsPersistenceBufferSize@] 
  -- An admissible amount of events the persistence layer may be lagging after
  -- the actual state of graph. 
  -- Until that amount is reached persistence of events is done asynchronously,
  -- thus reducing the time of execution of event.
  -- If you want the persisted state to always accomodate to the actual in-memory state,
  -- set this to @1@.
  -- 
  -- [@persistencePaths@] 
  -- Paths for persistence directories.
  -- 
  Mode_Local (Maybe (Int, Storage.Paths)) |
  -- |
  -- Connect to some server running on a host or a socket-file.
  Mode_Remote URL

-- |
-- Location of server.
data URL =
  -- | Path to the socket-file.
  URL_Socket FilePath |
  -- | Host name, port and password.
  URL_Host Text Int (Maybe ByteString)

-- | 
-- Determine paths from a unique name among all storages running on this machine. 
-- It will be used to set default values for storage paths under \"~\/.graph-db\/\[name\]\/\".
pathsFromName :: Text -> IO Storage.Paths
pathsFromName name = Storage.pathsFromDirectory ("~/.graph-db/" <> FilePath.fromText name)

-- |
-- Start the engine.
-- 
-- In case of local persisted mode, loads the latest state. 
-- Loading may take a while. 
-- Naturally, the time it takes is proportional to the size of database.
-- The startup time also depends on whether the engine was shutdown previously, 
-- since servicing of persistence files takes place on 'shutdownEngine'. 
startEngine :: (GraphDBTag t) => Graph.Root t -> Mode -> IO (Engine t)
startEngine root mode = case mode of
  Mode_Local (Just (eventsPersistenceBufferSize, storagePaths)) -> do
    eventsPersistenceBuffer <- IOQueue.start eventsPersistenceBufferSize
    (storage, graph) <- Storage.acquireAndLoad initGraph applyUnionEvent storagePaths
    return $ Engine_Persistent eventsPersistenceBuffer storage graph
    where
      initGraph = Graph.new root
      applyUnionEvent graph unionEvent = case unionEventTransaction unionEvent of
        Write write -> void $ Graph.runWrite graph write
        Read read -> error "Unexpected read-event"
  Mode_Local Nothing -> Engine_NonPersistent <$> (Graph.new root)
  Mode_Remote url -> Engine_Remote <$> Client.connect clientURL
    where
      clientURL = case url of
        URL_Host name port password -> Client.Host name port password
        URL_Socket path -> Client.Socket path

-- | 
-- The component managing datastructure, persistence and connection to remote server.
-- What it does exactly depends on its startup mode.
-- 
-- [@t@] A tag-type determining all the associated types with db.
data Engine t = 
  Engine_Remote (Client.Client (UnionEvent t) (UnionEventResult t)) |
  Engine_Persistent (IOQueue.IOQueue) (Storage.Storage (Graph.Graph t) (UnionEvent t)) (Graph.Graph t) |
  Engine_NonPersistent (Graph.Graph t)

shutdownEngine :: (GraphDBTag t) => Engine t -> IO ()
shutdownEngine engine = case engine of
  Engine_Persistent buffer storage graph -> do
    IOQueue.shutdown buffer
    Storage.checkpoint storage graph
    Storage.release storage
  Engine_NonPersistent _ -> return ()
  Engine_Remote client -> Client.disconnect client

-- | Shutdown engine without maintenance. 
shutdownEngine' :: (GraphDBTag t) => Engine t -> IO ()
shutdownEngine' engine = case engine of
  Engine_Persistent buffer storage graph -> do
    IOQueue.shutdown buffer
    Storage.release storage
  Engine_NonPersistent _ -> return ()
  Engine_Remote client -> Client.disconnect client


--------------------------------------------------------------------------------

-- | A packed transaction
data Transaction t a =
  Write (forall s. Graph.Write t s a) |
  Read (forall s. Graph.Read t s a)

instance Functor (Transaction t) where
  fmap f t = case t of
    Write write -> Write $ fmap f write
    Read read -> Read $ fmap f read

class 
  (Graph.GraphTag t, Serializable IO (UnionEvent t), Serializable IO (UnionEventResult t)) =>
  GraphDBTag t
  where
    data UnionEvent t
    data UnionEventResult t
    unionEventTransaction :: UnionEvent t -> Transaction t (UnionEventResult t)

class (Serializable IO (UnionEvent t), IsUnionEventResult t (EventResult t a)) => IsUnionEvent t a where
  type EventResult t a
  eventTransaction :: a -> Transaction t (EventResult t a)
  toUnionEvent :: a -> UnionEvent t
  fromUnionEvent :: UnionEvent t -> Maybe a

class IsUnionEventResult t a where
  toUnionEventResult :: a -> UnionEventResult t
  fromUnionEventResult :: UnionEventResult t -> Maybe a

runEvent :: (IsUnionEvent t e) => Engine t -> e -> IO (EventResult t e)
runEvent engine event = case engine of
  Engine_Persistent buffer storage graph -> case eventTransaction event of
    Write write -> do
      IOQueue.enqueue buffer $ Storage.persistEvent storage $ toUnionEvent event
      Graph.runWrite graph write
    Read read -> do
      Graph.runRead graph read
  Engine_NonPersistent graph -> case eventTransaction event of
    Write write -> Graph.runWrite graph write
    Read read -> Graph.runRead graph read
  Engine_Remote client -> 
    (Client.request client $ toUnionEvent event) >>=
    return . fromUnionEventResult >>=
    return . fromMaybe (error "Unexpected event result")

runUnionEvent :: 
  (GraphDBTag t, Serializable IO (UnionEvent t)) =>
  Engine t -> UnionEvent t -> IO (UnionEventResult t)
runUnionEvent engine unionEvent = case engine of
  Engine_Persistent buffer storage graph -> case unionEventTransaction unionEvent of
    Write write -> do
      IOQueue.enqueue buffer $ Storage.persistEvent storage unionEvent
      Graph.runWrite graph write
    Read read -> do
      Graph.runRead graph read
  Engine_NonPersistent graph -> case unionEventTransaction unionEvent of
    Write write -> Graph.runWrite graph write
    Read read -> Graph.runRead graph read
  Engine_Remote client -> Client.request client unionEvent

--------------------------------------------------------------------------------

-- | 
-- The settings of server.
data ServerMode = 
  -- | 
  -- A port to run the server on and a list of acceptable passwords.
  -- Empty list of passwords means a free access.
  -- 
  ServerMode_Host Int [ByteString] | 
  -- | 
  -- Path to the socket file.
  -- Since sockets are local no password-protection is required.
  -- 
  ServerMode_Socket FilePath

data Server t = Server {
  shutdownServer :: IO ()
}

startServer :: 
  (GraphDBTag t, Serializable IO (UnionEvent t), Serializable IO (UnionEventResult t)) =>
  Engine t -> ServerMode -> IO (Server t)
startServer engine serverMode = do
  acidServer <- Server.start (void . return) (5 * 60 * 10^6) acidServerMode processRequest
  return $ Server (shutdownServer acidServer)
  where
    acidServerMode = case serverMode of
      ServerMode_Socket path -> Server.Socket path
      ServerMode_Host port passwords -> Server.Host port passwords
    shutdownServer acidServer = Server.shutdown acidServer
    processRequest unionEvent = runUnionEvent engine unionEvent
