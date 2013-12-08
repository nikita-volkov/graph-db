{-# LANGUAGE UndecidableInstances #-}
-- NOTE: Alternative title: Client.
module GraphDB.Engine
  (
    -- * Configuration and maintenance
    Engine,
    start,
    shutdown,
    shutdown',
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
    Index(..),
    Value(..),
    Event(..),
    EventResult(..),
    FinalTransaction(..),
  )
  where

import GraphDB.Prelude hiding (Read, Write)
import qualified GraphDB.Engine.Node as Node
import qualified GraphDB.Engine.Dispatcher as Dispatcher; import GraphDB.Engine.Dispatcher (Dispatcher)
import qualified GraphDB.IOQueue as IOQueue; import GraphDB.IOQueue (IOQueue)
import qualified AcidIO.Storage as Storage
import qualified AcidIO.Server as Server
import qualified AcidIO.Client as Client
import qualified Filesystem.Path.CurrentOS as FilePath



-- | 
-- Engine running mode.
data Mode =
  -- | 
  -- Run in current process. 
  -- If no paths is provided, then there'll be no persistence.
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

-- NOTE: Alternative naming conventions: 
--  - TagIndex, TagValue, tagValueIndex ...
--  - Tag_Index, tag_valueIndex ...
class 
  (
    Serializable IO (UnionEventResult t),
    Serializable IO (UnionEvent t),
    Serializable IO (UnionValue t),
    Hashable (UnionType t),
    Eq (UnionType t),
    Hashable (UnionIndex t),
    Eq (UnionIndex t)
  ) => 
  Tag t 
  where
    type Root t
    data UnionIndex t
    data UnionValue t
    data UnionType t
    data UnionEvent t
    data UnionEventResult t
    unionIndexes :: UnionValue t -> UnionType t -> [UnionIndex t]
    unionIndexTargetType :: UnionIndex t -> UnionType t
    decomposeUnionValue :: UnionValue t -> (UnionType t, Any)
    composeUnionValue :: UnionType t -> Any -> UnionValue t
    unionEventFinalTransaction :: UnionEvent t -> FinalTransaction t (UnionEventResult t)

----------------
-- Adaptation of Node's API.
----------------

instance (Tag t) => Node.Type (UnionType t) where
  type Index (UnionType t) = UnionIndex t
  type Value (UnionType t) = UnionValue t
  indexes = unionIndexes
  decomposeValue = decomposeUnionValue
  composeValue = composeUnionValue
  targetType = unionIndexTargetType

type UnionNode t = Node.Node (UnionType t)

----------------

-- NOTE: Alternative naming convention: GraphEvent, GraphIndex, ...
class (EventResult t (Event_Result t e)) => Event t e where
  type Event_Result t e
  eventFinalTransaction :: e -> FinalTransaction t (Event_Result t e)
  packEvent :: e -> UnionEvent t

class EventResult t r where
  packEventResult :: r -> UnionEventResult t
  unpackEventResult :: UnionEventResult t -> Maybe r

class (Tag t) => Value t v where
  packValue :: v -> (UnionType t, UnionValue t)
  unpackValue :: UnionValue t -> Maybe v

unionType :: Value t v => v -> UnionType t
unionType v = let (ut, _) = packValue v in ut

unionValue :: Value t v => v -> UnionValue t
unionValue v = let (_, uv) = packValue v in uv


-- |
-- Defines a specific set of indexes on nodes of value /v'/ for nodes of value /v/.
-- 
-- E.g., an artist may be referred from a root by its UID and search terms,
-- however, for an album it may emit no indexes at all, and so may only
-- be reached as an element of a list of all linked artists.
-- 
-- If there is no instance of this class between two values, 
-- then the associated nodes cannot be linked.
-- 
-- NOTE: Instead of 'Reachable'
class (Index t (Edge_Index t v v'), Value t v, Value t v') => Edge t v v' where
  data Edge_Index t v v'
  indexes :: v' -> [Edge_Index t v v']
  indexes = const []

class (Tag t) => Index t i where
  packIndex :: i -> UnionIndex t

-- | 
-- A component managing datastructure, persistence and connection to remote server.
-- What it does exactly depends on its startup mode.
-- 
-- [@t@] A tag-type determining all the associated types with db.
data Engine t =
  Engine {
    -- | Run event.
    runEvent :: Event t e => e -> IO (Event_Result t e),
    -- | An internal function used by "GraphDB.Server".
    runUnionEvent :: UnionEvent t -> IO (UnionEventResult t),
    -- | Shutdown DB, releasing all acquired resources.
    -- Also performs a maintenance sequence (e.g., does checkpointing).
    shutdown :: IO (),
    -- | Shutdown without performing maintenance. 
    -- Affects only a local persisted mode, in which it won't do the checkpointing.
    shutdown' :: IO ()
  }

-- |
-- Start the engine.
-- 
-- In case of local persisted mode, loads the latest state. 
-- Loading may take a while. 
-- Naturally, the time it takes is proportional to the size of database.
-- The startup time also depends on whether the engine was shutdown previously, 
-- since servicing of persistence files takes place on 'shutdownEngine'. 
start :: forall t. (Tag t, Value t (Root t)) => Root t -> Mode -> IO (Engine t)
start rootValue mode = case mode of
  Mode_Local persistenceSettings -> do
    dispatcher <- Dispatcher.new
    case persistenceSettings of
      Nothing -> do
        root :: UnionNode t <- Node.new $ unionValue rootValue
        let
          runEvent :: Event t e => e -> IO (Event_Result t e)
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
          runEvent :: Event t e => e -> IO (Event_Result t e)
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
      runEvent :: forall e. Event t e => e -> IO (Event_Result t e)
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
-- Determine paths from a unique name among all storages running on this machine. 
-- It will be used to set default values for storage paths under \"~\/.graph-db\/\[name\]\/\".
pathsFromName :: Text -> IO Storage.Paths
pathsFromName name = Storage.pathsFromDirectory ("~/.graph-db/" <> FilePath.fromText name)




-- |
-- A transaction-local reference to node. 
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
newNode :: (Value t v) => v -> ReadOrWrite t s (Node t s v)
newNode = fmap Node . liftIO . Node.new . unionValue

-- |
-- Get all linked nodes with values of the provided type.
-- Supposed to be used like this:
-- 
-- > getTargetsByType (undefined :: Artist) ...
-- 
getTargetsByType :: (Edge t v v') => v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByType v (Node source) =
  liftIO $ map Node <$> Node.getTargetsByType source (unionType v)

-- |
-- Get target nodes reachable by the provided index.
getTargetsByIndex :: (Edge t v v') => Edge_Index t v v' -> Node t s v -> ReadOrWrite t s [Node t s v']
getTargetsByIndex i (Node source) = 
  liftIO $ map Node <$> Node.getTargetsByIndex source (packIndex i)

-- |
-- Add a link to the provided target node /v'/, 
-- while automatically generating all the indexes.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node was already there it would not be.
addTarget :: (Edge t v v') => Node t s v' -> Node t s v -> Write t s Bool
addTarget (Node target) (Node source) = 
  liftIO $ Node.addTarget source target

-- |
-- Remove the target node /v'/ and all its indexes from the source node /v/.
-- 
-- The result signals, whether the operation has actually been performed.
-- If the node was already there it would not be.
removeTarget :: (Edge t v v') => Node t s v' -> Node t s v -> Write t s Bool
removeTarget (Node target) (Node source) = 
  liftIO $ Node.removeTarget source target

-- | 
-- Get the value of the node.
getValue :: (Value t v) => Node t s v -> ReadOrWrite t s v
getValue (Node n) = 
  liftIO (Node.getValue n) >>= 
  return . unpackValue >>=
  return . fromMaybe (error "GraphDB.Engine.getValue: Unexpected value")

-- | 
-- Replace the value of the specified node.
setValue :: (Value t v) => v -> Node t s v -> Write t s ()
setValue a (Node n) = liftIO $ Node.setValue n (unionValue a)

-- |
-- Count the total amounts of distinct nodes and edges in the graph.
-- 
-- Requires traversal of the whole graph, so beware.
getStats :: ReadOrWrite t s (Int, Int)
getStats = do
  Node tn <- getRoot
  liftIO $ Node.getStats tn


