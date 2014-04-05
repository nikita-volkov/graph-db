-- |
-- An API, handling all storage tasks, which includes management of log-files, 
-- reading from and writing to them, creating checkpoints, archiving data and etc.
-- 
-- It does not know anything about transactions dispatch, nor does it handle any related problems.
-- 
module GraphDB.Storage 
  ( 
    -- * Configuration
    Paths,
    pathsFromDirectory,
    -- * Workflow
    Storage,
    acquireAndLoad,
    release,
    persistEvent,
    checkpoint,
    StorageException(..),

  ) 
  where

import GraphDB.Util.Prelude
import qualified Data.ByteString as ByteString
import qualified Pipes.Prelude as Pipes
import qualified Pipes.ByteString as PipesBS
import qualified GraphDB.Util.FileSystem as FileSystem
import qualified GraphDB.Storage.Rules as Rules


-- |
-- A component which manages persistence files, loads them and populates them.
data Storage a e = Storage {
  paths :: Paths,
  fileLockRef :: IORef (Maybe FileSystem.Lock),
  indexRef :: IORef Word,
  eventsFileHandleRef :: IORef (Maybe Handle)
}

-- |
-- Storage paths. Determines where to store logs, checkpoints, archive.
-- 
data Paths = Paths { 
  eventsDir :: FilePath, 
  checkpointsDir :: FilePath, 
  archiveDir :: FilePath,
  lockFile :: FilePath
}

data StorageException = 
  DeserializationFailure Text
  deriving (Typeable, Show, Eq, Ord)
instance Exception StorageException

-- |
-- Determine paths based on a provided root-directory.
pathsFromDirectory :: FilePath -> IO Paths
pathsFromDirectory dir = do
  base <- FileSystem.resolve dir
  return $ Paths base base (base <> "archive") (base <> ".lock")

-- |
-- Construct 'Storage' acquiring a lock over the directories.
-- 
acquire :: Paths -> IO (Storage a e)
acquire paths@Paths{..} = do
  createDirs
  storage <- Storage <$> pure paths <*> newIORef Nothing <*> newIORef 0 <*> newIORef Nothing
  -- TODO: put locks in all directories.
  acquireFileLock storage
  return storage
  where
    createDirs = 
      traverse_ FileSystem.createTree $ nub $ 
        [eventsDir, checkpointsDir, archiveDir, FileSystem.directory lockFile]

-- |
-- Construct 'Storage', locking managed directories and 
-- restoring the persisted data structure.
-- 
-- [@Usage@] 
-- 
-- > acquireAndLoad initValue replayEvent paths
-- 
-- [@initValue@] Get initial value. Gets called when there's no checkpoint.
-- 
-- [@replayEvent@] A function which applies events to this data structure.
-- 
-- [@paths@] Paths to storage directories.
-- 
acquireAndLoad :: (Serializable IO a, Serializable IO e) => IO a -> (a -> e -> IO ()) -> Paths -> IO (Storage a e, a)
acquireAndLoad initValue replayEvent paths = do
  p <- acquire paths
  a <- load p initValue replayEvent
  return (p, a)

-- |
-- Release acquired resources.
release :: Storage a e -> IO ()
release storage = do
  releaseEventsFileHandle storage
  releaseFileLock storage

load :: (Serializable IO a, Serializable IO e) => Storage a e -> IO a -> (a -> e -> IO ()) -> IO a
load storage@Storage{..} initValue replayEvent = do
  releaseEventsFileHandle storage -- Release a log, which we may now have to read from.
  (replayStartIndex, value) <- do
    r <- deserializeLatestCheckpoint
    case r of
      Just (index, value) -> do
        return (index, value)
      Nothing -> do
        (,) <$> pure 0 <*> initValue
  index <- do
    r <- replayEventsPastIndex value replayStartIndex
    return $ r ?: replayStartIndex + 1
  setIndex storage index
  return value
  where
    Paths{..} = paths
    deserializeLatestCheckpoint = do
      files <- 
        FileSystem.listFilesByExtension checkpointsDir Rules.checkpointExtension >>=
        return . sortWith (Down . fileIndex)
      fmap msum $ forM files $ \file -> (fmap . fmap) (fileIndex file, ) $ deserializeFile file
      where
        fileIndex = fromMaybe (error "Unparsable checkpoint filename") . Rules.checkpointFileIndex
        deserializeFile file = do
          r <- FileSystem.withFile file FileSystem.ReadMode $ \handle -> runEitherT $ Pipes.head $
            PipesBS.fromHandle handle >-> deserializingPipe
          either (throwIO . DeserializationFailure) return r
    replayEventsPastIndex value index = do
      files <- 
        FileSystem.listFilesByExtension eventsDir Rules.eventsExtension >>=
        return . map (fileIndex &&& id) >>=
        return . filter ((> index) . fst) >>=
        return . sortWith fst
      forM_ files (replayFileEvents . snd)
      return $ fmap fst $ lastZ files
      where
        fileIndex f = Rules.eventsFileIndex f ?: error "Unparsable event filename"
        replayFileEvents file = do
          FileSystem.withFile file FileSystem.ReadMode $ \handle -> do
            r <- runEitherT $ runEffect $ 
              PipesBS.fromHandle handle >->
              deserializingPipe >->
              (forever $ await >>= liftIO . replayEvent value)
            either (throwIO . DeserializationFailure) return r

-- |
-- Persist an event to disk.
-- Appends the supplied event to the currently active events' log-file.
-- 
persistEvent :: Serializable IO e => Storage a e -> e -> IO ()
persistEvent Storage{..} event = do
  handle <- fromMaybe raiseNoEventsFile <$> readIORef eventsFileHandleRef
  runEffect $ for (serializingProducer event) (liftIO . ByteString.hPutStr handle)
  where
    raiseNoEventsFile = error "TODO: implement raiseNoEventsFile"

-- |
-- Serialize and store the current state of data structure.
-- 
-- Switches the state of event-logging to a new file, 
-- so that all consecutive calls to 'persistEvent'
-- don't write to the latest log-file, which accomodates to the state of the data structure 
-- when this function gets called.
-- 
-- Cleans up after running.
-- 
checkpoint :: Serializable IO a => Storage a e -> a -> IO ()
checkpoint storage@Storage{..} value = do
  index <- readIORef indexRef
  setIndex storage $ succ index
  file <- return $ checkpointsDir <> Rules.checkpointFileName index
  FileSystem.withFile file FileSystem.WriteMode $ \handle -> runEffect $ 
    for (serializingProducer value) (liftIO . ByteString.hPutStr handle)
  cleanUp index
  where
    Paths{..} = paths
    -- |
    -- Delete checkpoints past index, considering them corrupt, and
    -- archive all preceding checkpoints and event-logs with index up to the specified (and including).
    cleanUp index = do
      do
        checkpointFiles <- FileSystem.listFilesByExtension checkpointsDir Rules.checkpointExtension
        let (outdated, current, corrupt) = 
              precedingMatchAndRemainder ((== index) . checkpointFileIndex) $ 
              sortWith checkpointFileIndex checkpointFiles
        forM_ corrupt FileSystem.removeFile
        forM_ outdated moveFileToArchive
      do
        eventsFiles <- FileSystem.listFilesByExtension eventsDir Rules.eventsExtension
        let (outdated, current) = 
              map snd *** map snd $
              break ((> index) . fst) $ 
              sortWith fst $ 
              map (eventsFileIndex &&& id) $ eventsFiles
        forM_ outdated moveFileToArchive
      where
        Paths{..} = paths
        checkpointFileIndex = fromMaybe (error "Unparsable checkpoint filename") . Rules.checkpointFileIndex
        eventsFileIndex = fromMaybe (error "Unparsable events' filename") . Rules.eventsFileIndex
        precedingMatchAndRemainder predicate list = case break predicate list of
          (preceding, match : remainder) | predicate match -> (preceding, Just match, remainder)
          (preceding, remainder) -> (preceding, Nothing, remainder)
        moveFileToArchive file = FileSystem.move file $ archiveDir <> FileSystem.filename file

acquireFileLock :: Storage a e -> IO ()
acquireFileLock storage@Storage{..} = do
  releaseFileLock storage
  lock <- FileSystem.acquireLock $ lockFile paths
  writeIORef fileLockRef $ Just lock

releaseFileLock :: Storage a e -> IO ()
releaseFileLock Storage{..} = do
  r <- readIORef fileLockRef
  case r of
    Just lock -> do
      FileSystem.releaseLock lock
      writeIORef fileLockRef Nothing
    Nothing -> return ()

setIndex :: Storage a e -> Word -> IO ()
setIndex storage@Storage{..} index = do
  writeIORef indexRef index
  acquireEventsFileHandle storage

acquireEventsFileHandle :: Storage a e -> IO ()
acquireEventsFileHandle storage@Storage{..} = do
  releaseEventsFileHandle storage
  file <- do
    index <- readIORef indexRef
    return $ eventsDir <> Rules.eventsFileName index
  do
    exists <- FileSystem.getExists file
    when exists $ error $ "Trying to switch logging to an already existing file: " ++ show file
  writeIORef eventsFileHandleRef . Just =<< FileSystem.openFile file FileSystem.WriteMode
  where 
    Paths{..} = paths

releaseEventsFileHandle :: Storage a e -> IO ()
releaseEventsFileHandle Storage{..} = 
  readIORef eventsFileHandleRef >>= 
  traverse_ (\handle -> hClose handle >> writeIORef eventsFileHandleRef Nothing)
