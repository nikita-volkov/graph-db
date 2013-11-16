module GraphDB.API
  (
    -- * DB Engine
    Engine,
    startEngine,
    shutdownEngine,
    runEvent,
    Mode(..),
    Storage.pathsFromName,
    Storage.pathsFromDirectory,
    Storage.Paths,
    URL(..),

    -- * Transactions
    Write,
    Read,
    EdgeTo,
    NodeRef,

    -- ** Transaction building blocks
    getRoot,
    newNode,
    getTargets,
    getValue,
    setValue,
    insertEdge,
    deleteEdge,

    -- * Server
    ServerMode(..),
    Server,
    startServer,
    shutdownServer,

    -- * Boilerplate
    Tag(..),
    Event(..),
    Transaction(..),
    IsMemberValueOf(..),
    IsMemberEdgeOf(..),
    IsMemberEventOf(..),
    IsMemberEventResultOf(..),
  ) 
  where

import GraphDB.Prelude hiding (Read, log)
import qualified GraphDB.IOQueue as IOQueue; import GraphDB.IOQueue (IOQueue)
import qualified GraphDB.Graph as Graph
import qualified GraphDB.Graph.Transaction as Transaction
import qualified AcidIO.Storage as Storage
import qualified AcidIO.Server as Server
import qualified AcidIO.Client as Client


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
-- Start the engine.
-- 
-- In case of local persisted mode, loads the latest state. 
-- Loading may take a while. Naturally, the time it takes is proportional to the size of database.
-- The startup time also depends on whether the engine was shutdown previously, 
-- since servicing of persistence files takes place on 'shutdownEngine'. 
startEngine ::
  (
    Tag t,
    IsMemberValueOf () t, 
    Hashable (MemberEdge t), 
    Eq (MemberEdge t), 
    Serializable (MemberEdge t) IO, 
    Serializable (MemberValue t) IO, 
    Serializable (MemberEvent t) IO, 
    Serializable (MemberEventResult t) IO 
  ) => 
  Mode -> 
  IO (Engine t)
startEngine mode = case mode of
  Mode_Local (Just (eventsPersistenceBufferSize, storagePaths)) -> do
    eventsPersistenceBuffer <- IOQueue.start eventsPersistenceBufferSize
    (storage, graph) <- Storage.acquireAndLoad initGraph applyMemberEvent storagePaths
    return $ Engine_Persistent eventsPersistenceBuffer storage graph
    where
      initGraph = Graph.new $ toMemberValue ()
      applyMemberEvent graph memberEvent = case memberEventTransaction memberEvent of
        Write write -> void $ Graph.runWrite graph write
        Read read -> error "Unexpected read-event"
  Mode_Local Nothing -> Engine_NonPersistent <$> (Graph.new $ toMemberValue ())
  Mode_Remote url -> Engine_Remote <$> Client.connect clientURL
    where
      clientURL = case url of
        URL_Host name port password -> Client.Host name port password
        URL_Socket path -> Client.Socket path

-- | The component managing datastructure, persistence and connection to remote server.
-- What it does exactly depends on its startup mode.
data Engine t = 
  Engine_Remote (Client.Client (MemberEvent t) (MemberEventResult t)) |
  Engine_Persistent (IOQueue.IOQueue) (Storage.Storage (Graph t) (MemberEvent t)) (Graph t) |
  Engine_NonPersistent (Graph t)

shutdownEngine ::
  ( Hashable (MemberEdge t), Serializable (MemberEdge t) IO, Eq (MemberEdge t), 
    Hashable (MemberValue t), Serializable (MemberValue t) IO, Eq (MemberValue t) ) =>
  Engine t -> IO ()
shutdownEngine engine = case engine of
  Engine_Persistent buffer storage graph -> do
    IOQueue.shutdown buffer
    Storage.checkpoint storage graph
    Storage.release storage
  Engine_NonPersistent _ -> return ()
  Engine_Remote client -> Client.disconnect client


--------------------------------------------------------------------------------

-- |
-- Properties of an edge from any source node to /target/ node. E.g.:
-- 
-- @
-- data instance GraphDB.EdgeTo Artist = ArtistOf | ArtistOfByName Text
-- data instance GraphDB.EdgeTo Genre = GenreOf
-- @
-- 
data family EdgeTo target

newtype NodeRef t s a = NodeRef (Graph.NodeRef (MemberValue t) (MemberEdge t) s)

getRoot :: Read t s (NodeRef t s a)
getRoot = NodeRef `liftM` Graph.getRoot

newNode :: (IsMemberValueOf a t) => a -> Read t s (NodeRef t s a)
newNode value = NodeRef `liftM` Graph.newNode (toMemberValue value)

getTargets :: (IsMemberEdgeOf (EdgeTo b) t, Hashable (MemberEdge t), Eq (MemberEdge t)) => 
              EdgeTo b -> NodeRef t s a -> Read t s [NodeRef t s b]
getTargets edge (NodeRef ref) = map NodeRef `liftM` Graph.getTargets (toMemberEdge edge) ref

getValue :: (IsMemberValueOf a t) => NodeRef t s a -> Read t s a
getValue (NodeRef ref) = liftM (fromMaybe bug . fromMemberValue) $ Graph.getValue ref where
  bug = error "Unexpected value. This is a bug. Please report it."

setValue :: (IsMemberValueOf a t) => NodeRef t s a -> a -> Write t s ()
setValue (NodeRef ref) value = Graph.setValue ref (toMemberValue value)

insertEdge :: (IsMemberEdgeOf (EdgeTo b) t, Hashable (MemberEdge t), Eq (MemberEdge t)) => 
              NodeRef t s a -> EdgeTo b -> NodeRef t s b -> Write t s ()
insertEdge (NodeRef ref1) edge (NodeRef ref2) = Graph.insertEdge ref1 (toMemberEdge edge) ref2

deleteEdge :: (IsMemberEdgeOf (EdgeTo b) t, Hashable (MemberEdge t), Eq (MemberEdge t)) => 
              NodeRef t s a -> EdgeTo b -> NodeRef t s b -> Write t s ()
deleteEdge (NodeRef ref1) edge (NodeRef ref2) = Graph.deleteEdge ref1 (toMemberEdge edge) ref2


--------------------------------------------------------------------------------

-- | A packed transaction
data Transaction t a =
  Write (forall s. Write t s a) |
  Read (forall s. Read t s a)

instance Functor (Transaction t) where
  fmap f t = case t of
    Write write -> Write $ fmap f write
    Read read -> Read $ fmap f read

type Write t s a = Graph.Write (MemberValue t) (MemberEdge t) s a
type Read t s a = forall r r'. (Graph.Reads r, r' ~ r (MemberValue t) (MemberEdge t) s, MonadIO r', Applicative r', Functor r') => r' a

type Graph t = Graph.Graph (MemberValue t) (MemberEdge t)

class Tag t where
  data MemberEvent t
  data MemberEventResult t
  data MemberValue t
  data MemberEdge t
  memberEventTransaction :: MemberEvent t -> Transaction t (MemberEventResult t)

class Event e t where
  type EventResult e t
  eventTransaction :: e -> Transaction t (EventResult e t)

runEvent :: 
  (Event e t, IsMemberEventOf e t, Serializable (MemberEvent t) IO, IsMemberEventResultOf (EventResult e t) t) => 
  Engine t -> e -> IO (EventResult e t)
runEvent engine event = case engine of
  Engine_Persistent buffer storage graph -> case eventTransaction event of
    Write write -> do
      IOQueue.enqueue buffer $ Storage.persistEvent storage $ toMemberEvent event
      Graph.runWrite graph write
    Read read -> do
      Graph.runRead graph read
  Engine_NonPersistent graph -> case eventTransaction event of
    Write write -> Graph.runWrite graph write
    Read read -> Graph.runRead graph read
  Engine_Remote client -> 
    (Client.request client $ toMemberEvent event) >>=
    return . fromMemberEventResult >>=
    return . fromMaybe (error "Unexpected event result")

runMemberEvent :: 
  (Tag t, Serializable (MemberEvent t) IO) =>
  Engine t -> MemberEvent t -> IO (MemberEventResult t)
runMemberEvent engine memberEvent = case engine of
  Engine_Persistent buffer storage graph -> case memberEventTransaction memberEvent of
    Write write -> do
      IOQueue.enqueue buffer $ Storage.persistEvent storage memberEvent
      Graph.runWrite graph write
    Read read -> do
      Graph.runRead graph read
  Engine_NonPersistent graph -> case memberEventTransaction memberEvent of
    Write write -> Graph.runWrite graph write
    Read read -> Graph.runRead graph read
  Engine_Remote client -> Client.request client memberEvent

class IsMemberEventOf a t where
  toMemberEvent :: a -> MemberEvent t
  fromMemberEvent :: MemberEvent t -> Maybe a

class IsMemberEventResultOf a t where
  toMemberEventResult :: a -> MemberEventResult t
  fromMemberEventResult :: MemberEventResult t -> Maybe a

-- |
-- Functions for converting a value to and from a union value.
class IsMemberValueOf a t where
  toMemberValue :: a -> MemberValue t
  fromMemberValue :: MemberValue t -> Maybe a

-- |
-- Functions for converting an edge to and from a union value.
class IsMemberEdgeOf a t where
  toMemberEdge :: a -> MemberEdge t
  fromMemberEdge :: MemberEdge t -> Maybe a

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
  (Tag t, Serializable (MemberEvent t) IO, Serializable (MemberEventResult t) IO) =>
  Engine t -> ServerMode -> IO (Server t)
startServer engine serverMode = do
  acidServer <- Server.start (void . return) (5 * 60 * 10^6) acidServerMode processRequest
  return $ Server (shutdownServer acidServer)
  where
    acidServerMode = case serverMode of
      ServerMode_Socket path -> Server.Socket path
      ServerMode_Host port passwords -> Server.Host port passwords
    shutdownServer acidServer = Server.shutdown acidServer
    processRequest memberEvent = runMemberEvent engine memberEvent
