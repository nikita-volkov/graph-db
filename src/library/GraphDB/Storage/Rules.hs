module GraphDB.Storage.Rules where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.FileSystem as FS


checkpointExtension :: Text
checkpointExtension = "checkpoint"

eventsExtension :: Text
eventsExtension = "events"

checkpointFileName :: Word -> FilePath
checkpointFileName index = 
  FS.decodeString (show index) `FS.addExtension` checkpointExtension

eventsFileName :: Word -> FilePath
eventsFileName index = 
  FS.decodeString (show index) `FS.addExtension` eventsExtension

checkpointFileIndex :: FilePath -> Maybe Word 
checkpointFileIndex file = readMaybe $ FS.encodeString $ FS.basename file

eventsFileIndex :: FilePath -> Maybe Word
eventsFileIndex file = readMaybe $ FS.encodeString $ FS.basename file
