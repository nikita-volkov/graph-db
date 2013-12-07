{-# LANGUAGE UndecidableInstances #-}
-- NOTE: Alternative title: Client.
module GraphDB.Engine
  -- (
  --   Graph,
  --   new,
  --   UnionNode.NodeValue(..),
  --   runWrite,
  --   runRead,
  --   Read,
  --   Write,
  --   getRoot,
  --   newNode,
  --   getTargetsByType,
  --   getTargetsByIndex,
  --   addTarget,
  --   removeTarget,
  --   getValue,
  --   setValue,
  --   getStats,
  -- )
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
    unionValueIndexes :: UnionType t -> UnionValue t -> [UnionIndex t]
    data UnionEvent t
    data UnionEventResult t
    unionEventFinalTransaction :: UnionEvent t -> FinalTransaction t (UnionEventResult t)

----------------
-- Adaptation of Node's API.
----------------

instance (Tag t) => Node.Type (UnionType t) where
  type Index (UnionType t) = UnionIndex t
  type Value (UnionType t) = UnionValue t

type UnionNode t = Node.Node (UnionType t)

----------------

-- NOTE: Alternative naming convention: GraphEvent, GraphIndex, ...
class (EventResult t (Event_Result t e)) => Event t e where
  type Event_Result t e
  event_finalTransaction :: e -> FinalTransaction t (Event_Result t e)
  event_toUnionEvent :: e -> UnionEvent t

class EventResult t r where
  eventResult_unpack :: UnionEventResult t -> Maybe r

class (Tag t) => Value t v where
  value_toUnionValue :: v -> UnionValue t

-- NOTE: Instead of 'Reachable'
class (Index t (Edge_Index t v v')) => Edge t v v' where
  data Edge_Index t v v'
  edge_indexes :: v' -> [Edge_Index t v v']

class Index t i where
  index_toUnionIndex :: i -> UnionIndex t

data Engine t =
  Engine {
    runEvent :: Event t e => e -> IO (Event_Result t e),
    shutdown :: IO ()
  }

startEngine :: forall t. (Tag t, Value t (Root t)) => Root t -> Mode -> IO (Engine t)
startEngine rootValue mode = case mode of
  Mode_Local persistenceSettings -> do
    dispatcher <- Dispatcher.new
    case persistenceSettings of
      Nothing -> do
        root :: UnionNode t <- Node.new $ value_toUnionValue rootValue
        let
          runEvent :: Event t e => e -> IO (Event_Result t e)
          runEvent e = 
            case event_finalTransaction e of
              FinalTransaction_Write (Write rootToIO) -> 
                Dispatcher.runWrite dispatcher $ rootToIO root
              FinalTransaction_Read (Read rootToIO) -> 
                Dispatcher.runRead dispatcher $ rootToIO root
        return $ Engine runEvent (return ())
      Just (eventsPersistenceBufferSize, storagePaths) -> do
        eventsPersistenceBuffer <- IOQueue.start eventsPersistenceBufferSize
        let
          initRoot :: IO (UnionNode t)
          initRoot = Node.new $ value_toUnionValue rootValue
          replayUnionEvent root = unionEventFinalTransaction >>> \case
            FinalTransaction_Write (Write rootToIO) -> 
              void $ Dispatcher.runWrite dispatcher $ rootToIO root
            FinalTransaction_Read _ -> error "Attempt to replay a read-event"
        (storage, root) <- Storage.acquireAndLoad initRoot replayUnionEvent storagePaths
        let
          runEvent :: Event t e => e -> IO (Event_Result t e)
          runEvent e = case event_finalTransaction e of
            FinalTransaction_Write (Write rootToIO) -> do
              IOQueue.enqueue eventsPersistenceBuffer $ do
                Storage.persistEvent storage $ event_toUnionEvent e 
              Dispatcher.runWrite dispatcher $ rootToIO root
            FinalTransaction_Read (Read rootToIO) -> do
              Dispatcher.runRead dispatcher $ rootToIO root
          shutdown = do
            IOQueue.shutdown eventsPersistenceBuffer
            Storage.checkpoint storage root
            Storage.release storage
        return $ Engine runEvent shutdown
  Mode_Remote url -> do
    client <- Client.connect clientURL
    let
      runUnionEvent :: UnionEvent t -> IO (UnionEventResult t)
      runUnionEvent = Client.request client
      runEvent :: forall e. Event t e => e -> IO (Event_Result t e)
      runEvent e = 
        runUnionEvent (event_toUnionEvent e) >>=
        return . eventResult_unpack >>=
        return . fromMaybe (error "Unexpected event result")
      shutdown = do
        Client.disconnect client
    return $ Engine runEvent shutdown
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
