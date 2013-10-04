-- |
-- A component handling all persistence tasks, which includes management of log-files, 
-- reading from and writing to them, creating checkpoints, archiving data and etc.
-- 
-- It does not know anything about transactions dispatch, nor does it handle any related problems.
-- 
module TPM.GraphDB.Storage 
  ( 
    Storage,
    Config,
    new, 
    log, 
    replayLogs,
    checkpoint,
    restoreFromCheckpoint
  ) 
  where

import TPM.GraphDB.Prelude hiding (State, log)
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal


-- |
-- Configuration and state of the component.
-- 
-- /tag/ is required for identifying the type instance of 'Event'.
-- 
data Storage tag = Storage Config (MVar State)

-- |
-- Configuration of the component.
-- 
-- TODO: to implement.
-- 
data Config

-- |
-- Internal state of the component.
-- 
-- TODO: to implement.
-- 
data State

-- |
-- Instantiate the component with the configuration info.
-- 
new :: Config -> IO (Storage tag)
new = undefined


-- |
-- A tagging information of some modification to the data structure.
-- This information is used later for replaying a log of those modifications to restore the data
-- to some state.
-- 
-- /tag/ is required for identifying the /type instance/.
-- 
type family Event tag

-- |
-- Persist an 'Event' to disk.
-- 
-- Appends the supplied event to the currently active log-file.
-- 
log :: (SafeCopy (Event tag)) => Storage tag -> Event tag -> IO ()
log = undefined

-- |
-- Replay all managed event-logs of the supplied 'Storage' with a function, 
-- which executes each 'Event' in 'IO'. Allows to restore a data structure in 'IO'. 
-- 
-- To completely restore the data structure this function should be used in combination with
-- 'restoreFromCheckpoint'. E.g.:
-- 
-- @
-- restoreMyData :: Storage tag -> IO MyData
-- restoreMyData storage = do
--   myData <- Storage.restoreFromCheckpoint storage MyData.deserialize
--   Storage.replay storage (void . MyData.runEvent myData)
--   return myData
-- @
-- 
replayLogs :: (SafeCopy (Event tag)) => Storage tag -> (Event tag -> IO ()) -> IO ()
replayLogs = undefined

-- |
-- Serialize and store the current state of data structure.
-- 
-- Switches the state of event-logging to a new file, 
-- so that all consecutive (including concurrent) calls to 'log'
-- don't write to the latest log file, which accomodates to the state of the data structure 
-- when this function gets called.
-- 
-- On finish moves all outdated event-logs and all preceding checkpoints into /archive/ directory.
-- 
checkpoint :: Storage tag -> IO Cereal.Put -> IO ()
checkpoint = undefined

-- |
-- Restore a data structure from the latest checkpoint using the supplied deserialization function.
-- 
-- To completely restore the data structure this function should be used in combination with
-- 'replayLogs'. For example see 'replayLogs'.
-- 
restoreFromCheckpoint :: Storage tag -> (ByteString -> IO (Cereal.Result a)) -> IO a
restoreFromCheckpoint = undefined


