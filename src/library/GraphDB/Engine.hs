module GraphDB.Engine
  (
    -- * Configuration and maintenance
    Engine,
    startEngine,
    shutdownEngine,
    shutdownEngine',
    runEvent,
    runUnionEvent,
    Mode(..),
    pathsFromName,
    Storage.pathsFromDirectory,
    Storage.Paths,
    URL(..),

    -- * Transactions
    Write,
    Read,
    ReadOrWrite,
    Transaction,
    Node,
    Edge(..),
    getRoot,
    newNode,
    getTargetsByType,
    getTargetsByIndex,
    addTarget,
    removeTarget,
    getValue,
    setValue,
    getStats,

    -- * Boilerplate
    Tag(..),
    TagEdge(..),
    TagIndex(..),
    TagValue(..),
    TagEvent(..),
    TagEventResult(..),
    FinalTransaction(..),

    -- * Server
    ServerMode(..), 
    Server, 
    shutdownServer, 
    startServer,
  )
  where

import GraphDB.Util.Prelude hiding (Read, Write)
import qualified GraphDB.Engine.Node as Node
import qualified GraphDB.Engine.Dispatcher as Dispatcher; import GraphDB.Engine.Dispatcher (Dispatcher)
import qualified GraphDB.Util.IOQueue as IOQueue; import GraphDB.Util.IOQueue (IOQueue)
import qualified GraphDB.Storage as Storage
import qualified GraphDB.Server as Server
import qualified GraphDB.Client as Client
import qualified Filesystem.Path.CurrentOS as FilePath



-- | 
-- Engine running mode.
data Mode =
  -- | 
  -- Run in the current process. 
  -- If no paths is provided, then there'll be no persistence.
  -- 
  -- [@Usage@]
  -- 
  -- > Mode_Local (Just (eventsPersistenceBufferSize, persistencePaths))
  -- 
  -- [@eventsPersistenceBufferSize@] 
  -- An admissible amount of events the persistence layer may be lagging after
  -- the actual state of the graph. 
  -- Until that amount is reached the persistence of events is done asynchronously,
  -- thus reducing the time of execution of events.
  -- If you want the persisted state to always accomodate to the actual in-memory state,
  -- set this to @1@. 
  -- Thus you will ensure the persistence of events is always done synchronously.
  -- 
  -- [@persistencePaths@] 
  -- Paths to persistence directories.
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
-- A component, managing datastructure, persistence and connection to remote server.
-- What it does exactly depends on its startup mode.
-- 
-- [@t@] A tag-type determining all the type-level settings of the DB.
data Engine t =
  Engine {
    -- | Run event.
    runEvent :: TagEvent t e => e -> IO (TagEvent_Result t e),
    -- | An internal function used by "GraphDB.Server".
    runUnionEvent :: UnionEvent t -> IO (UnionEventResult t),
    -- | Shutdown DB, releasing all acquired resources.
    -- Also performs a maintenance sequence (e.g., does checkpointing).
    shutdownEngine :: IO (),
    -- | Shutdown without performing maintenance. 
    -- Affects only a local persisted mode, in which it won't do the checkpointing.
    shutdownEngine' :: IO ()
  }

-- |
-- Start the engine.
-- 
-- In case of a local persisted mode, loads the latest state. 
-- Loading may take a while. 
-- Naturally, the time it takes is proportional to the size of the database.
-- The startup time also depends on whether the engine was shutdown previously, 
-- since servicing of persistence files takes place then. 
startEngine :: forall t. (Tag t, TagValue t (Root t)) => Root t -> Mode -> IO (Engine t)
startEngine rootValue mode = case mode of
  Mode_Local persistenceSettings -> do
    dispatcher <- Dispatcher.new
    case persistenceSettings of
      Nothing -> do
        root :: UnionNode t <- Node.new $ unionValue rootValue
        let
          runEvent :: TagEvent t e => e -> IO (TagEvent_Result t e)
          runEvent = eventFinalTransaction >>> \case
            FinalTransaction_Write (Write rootToIO) -> 
              Dispatcher.runWrite dispatcher $ rootToIO root
            FinalTransaction_Read (Read rootToIO) -> 
              Dispatcher.runRead dispatcher $ rootToIO root
          runUnionEvent = unionEventFinalTransaction >>> \case
            FinalTransaction_Write (Write rootToIO) -> 
              Dispatcher.runWrite dispatcher $ rootToIO root
            FinalTransaction_Read (Read rootToIO) -> 
              Dispatcher.runRead dispatcher $ rootToIO root
        return $ Engine runEvent runUnionEvent (return ()) (return ())
      Just (eventsPersistenceBufferSize, storagePaths) -> do
        eventsPersistenceBuffer <- IOQueue.start eventsPersistenceBufferSize
        let
          initRoot :: IO (UnionNode t)
          initRoot = Node.new $ unionValue rootValue
          replayUnionEvent root = unionEventFinalTransaction >>> \case
            FinalTransaction_Write (Write rootToIO) -> 
              void $ Dispatcher.runWrite dispatcher $ rootToIO root
            FinalTransaction_Read _ -> error "Attempt to replay a read-event"
        (storage, root) <- Storage.acquireAndLoad initRoot replayUnionEvent storagePaths
        let
          runEvent :: TagEvent t e => e -> IO (TagEvent_Result t e)
          runEvent e = case eventFinalTransaction e of
            FinalTransaction_Write (Write rootToIO) -> do
              IOQueue.enqueue eventsPersistenceBuffer $ do
                Storage.persistEvent storage $ packEvent e 
              Dispatcher.runWrite dispatcher $ rootToIO root
            FinalTransaction_Read (Read rootToIO) -> do
              Dispatcher.runRead dispatcher $ rootToIO root
          runUnionEvent e = case unionEventFinalTransaction e of
            FinalTransaction_Write (Write rootToIO) -> do
              IOQueue.enqueue eventsPersistenceBuffer $ do
                Storage.persistEvent storage e 
              Dispatcher.runWrite dispatcher $ rootToIO root
            FinalTransaction_Read (Read rootToIO) -> do
              Dispatcher.runRead dispatcher $ rootToIO root
          shutdown = do
            IOQueue.shutdown eventsPersistenceBuffer
            Storage.checkpoint storage root
            Storage.release storage
          shutdown' = do
            IOQueue.shutdown eventsPersistenceBuffer
            Storage.release storage
        return $ Engine runEvent runUnionEvent shutdown shutdown'
  Mode_Remote url -> do
    client <- Client.connect clientURL
    let
      runUnionEvent :: UnionEvent t -> IO (UnionEventResult t)
      runUnionEvent = Client.request client
      runEvent :: forall e. TagEvent t e => e -> IO (TagEvent_Result t e)
      runEvent e = 
        runUnionEvent (packEvent e) >>=
        return . unpackEventResult >>=
        return . fromMaybe (error "Unexpected event result")
      shutdown = do
        Client.disconnect client
    return $ Engine runEvent runUnionEvent shutdown shutdown
    where
      clientURL = case url of
        URL_Host name port password -> Client.Host name port password
        URL_Socket path -> Client.Socket path

-- |
-- Defines a specific set of indexes, which nodes of value /v'/ emit to nodes of value /v/.
-- 
-- If the indexes list is empty, 
-- the node may still be reached thru 'getTargetsByType'.
-- 
-- If there is no instance of this class between two values, 
-- then the associated nodes cannot be linked.
-- 
class Edge v v' where
  data Index v v'
  indexes :: v' -> [Index v v']
  indexes = const []

----------------
-- Boilerplate types.
----------------

class 
  (
    Serializable IO (UnionEventResult t),
    Serializable IO (UnionEvent t),
    Serializable IO (UnionValue t),
    Hashable (UnionValueType t),
    Eq (UnionValueType t),
    Hashable (UnionIndex t),
    Eq (UnionIndex t)
  ) => 
  Tag t 
  where
    type Root t
    data UnionIndex t
    data UnionValue t
    data UnionValueType t
    data UnionEvent t
    data UnionEventResult t
    unionIndexes :: UnionValue t -> UnionValueType t -> [UnionIndex t]
    unionIndexTargetType :: UnionIndex t -> UnionValueType t
    decomposeUnionValue :: UnionValue t -> (UnionValueType t, Any)
    composeUnionValue :: UnionValueType t -> Any -> UnionValue t
    unionEventFinalTransaction :: UnionEvent t -> FinalTransaction t (UnionEventResult t)

class (TagEventResult t (TagEvent_Result t e)) => TagEvent t e where
  type TagEvent_Result t e
  eventFinalTransaction :: e -> FinalTransaction t (TagEvent_Result t e)
  packEvent :: e -> UnionEvent t

class TagEventResult t r where
  packEventResult :: r -> UnionEventResult t
  unpackEventResult :: UnionEventResult t -> Maybe r

class (Tag t) => TagValue t v where
  packValue :: v -> (UnionValueType t, UnionValue t)
  unpackValue :: UnionValue t -> Maybe v

class (Tag t) => TagIndex t i where
  packIndex :: i -> UnionIndex t

type TagEdge t v v' = (TagValue t v, TagValue t v', TagIndex t (Index v v'), Edge v v')

unionType :: TagValue t v => v -> UnionValueType t
unionType v = let (ut, _) = packValue v in ut

unionValue :: TagValue t v => v -> UnionValue t
unionValue v = let (_, uv) = packValue v in uv

----------------
-- Adaptation of Node's API.
----------------

instance (Tag t) => Node.Type (UnionValueType t) where
  type Index (UnionValueType t) = UnionIndex t
  type Value (UnionValueType t) = UnionValue t
  indexes = unionIndexes
  decomposeValue = decomposeUnionValue
  composeValue = composeUnionValue
  targetType = unionIndexTargetType

type UnionNode t = Node.Node (UnionValueType t)

---------------

-- |
-- A transaction-local reference to the actual node of the graph-datastructure.
-- 
-- The /s/ is a state-thread making the escape of nodes from transaction
-- impossible. Much inspired by the realization of 'ST'.
newtype Node t s v = Node (UnionNode t) deriving (Eq)

-- | Support for common operations of a transaction.
class Transaction t where
  getRootUnionNode :: t tag s (UnionNode tag)

instance Transaction Write where
  getRootUnionNode = Write $ \z -> return z

instance Transaction Read where
  getRootUnionNode = Read $ \z -> return z


-- |
-- A write and read transaction.
-- 
newtype Write t s z = Write (UnionNode t -> IO z)

instance MonadIO (Write t s) where
  liftIO io = Write $ \_ -> io

instance Monad (Write t s) where
  return a = Write $ \_ -> return a
  writeA >>= aToWriteB = Write rootToIO where
    rootToIO root = ioA >>= aToIOB where
      Write rootToIOA = writeA
      ioA = rootToIOA root
      aToIOB a = ioB where
        Write rootToIOB = aToWriteB a
        ioB = rootToIOB root

instance Applicative (Write t s) where
  pure = return
  (<*>) = ap

instance Functor (Write t s) where
  fmap = liftM


-- |
-- A read-only transaction. Gets executed concurrently.
-- 
newtype Read t s z = Read (UnionNode t -> IO z)

instance MonadIO (Read t s) where
  liftIO io = Read $ \_ -> io

instance Monad (Read t s) where
  return a = Read $ \_ -> return a
  readA >>= aToReadB = Read rootToIO where
    rootToIO root = ioA >>= aToIOB where
      Read rootToIOA = readA
      ioA = rootToIOA root
      aToIOB a = ioB where
        Read rootToIOB = aToReadB a
        ioB = rootToIOB root

instance Applicative (Read t s) where
  pure = return
  (<*>) = ap

instance Functor (Read t s) where
  fmap = liftM


-- |
-- An abstract type. 
-- Transactions of this type can be composed with both 'Read' and 'Write'.
type ReadOrWrite t s z = 
  forall tr. (Transaction tr, MonadIO (tr t s), Applicative (tr t s)) => tr t s z


-- | A packed uncomposable specific transaction with information about its type.
-- NOTE: Alternative titles: EventTransaction, UnionTransaction
data FinalTransaction t a =
  FinalTransaction_Write (forall s. Write t s a) |
  FinalTransaction_Read (forall s. Read t s a)

instance Functor (FinalTransaction t) where
  fmap f t = case t of
    FinalTransaction_Write write -> FinalTransaction_Write $ fmap f write
    FinalTransaction_Read read -> FinalTransaction_Read $ fmap f read


-- |
-- Get the root node.
getRoot :: ReadOrWrite t s (Node t s (Root t))
getRoot = fmap Node getRootUnionNode

-- |
-- Create a new node. 
-- 
-- This node won't get stored if you don't insert at least a single edge 
-- from another stored node to it.
newNode :: (TagValue t v) => v -> ReadOrWrite t s (Node t s v)
newNode = fmap Node . liftIO . Node.new . unionValue

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType (undefined :: Artist) ...
-- 
getTargetsByType :: (TagEdge t v v') => v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByType v (Node source) =
  liftIO $ map Node <$> Node.getTargetsByType source (unionType v)

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: (TagEdge t v v') => Index v v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByIndex i (Node source) = 
  liftIO $ map Node <$> Node.getTargetsByIndex source (packIndex i)

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node was already there it will return 'False'.
addTarget :: (TagEdge t v v') => Node t s v' -> Node t s v -> Write t s Bool
addTarget (Node target) (Node source) = 
  liftIO $ Node.addTarget source target

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node was not found it will return 'False'.
removeTarget :: (TagEdge t v v') => Node t s v' -> Node t s v -> Write t s Bool
removeTarget (Node target) (Node source) = 
  liftIO $ Node.removeTarget source target

-- | 
-- Get the value of the node.
getValue :: (TagValue t v) => Node t s v -> ReadOrWrite t s v
getValue (Node n) = 
  liftIO (Node.getValue n) >>= 
  return . unpackValue >>=
  return . fromMaybe (error "GraphDB.Engine.getValue: Unexpected value")

-- | 
-- Replace the value of the specified node.
setValue :: (TagValue t v) => v -> Node t s v -> Write t s ()
setValue a (Node n) = liftIO $ Node.setValue n (unionValue a)

-- |
-- Count the total amounts of distinct nodes and edges in the graph.
-- 
-- Requires traversal of the whole graph, so beware.
getStats :: ReadOrWrite t s (Int, Int)
getStats = do
  Node tn <- getRoot
  liftIO $ Node.getStats tn



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
  (Tag t, Serializable IO (UnionEvent t), Serializable IO (UnionEventResult t)) =>
  Engine t -> ServerMode -> IO (Server t)
startServer engine serverMode = do
  acidServer <- Server.start (void . return) (5 * 60 * 10^6) acidServerMode processRequest
  let
    shutdown = Server.shutdown acidServer
  return $ Server shutdown
  where
    acidServerMode = case serverMode of
      ServerMode_Socket path -> Server.Socket path
      ServerMode_Host port passwords -> Server.Host port passwords
    processRequest unionEvent = runUnionEvent engine unionEvent
