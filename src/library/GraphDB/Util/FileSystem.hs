-- |
-- Utilities for dealing with 'FilePath'.
-- 
module GraphDB.Util.FileSystem
  ( 
    module Filesystem,
    module Filesystem.Path.CurrentOS,
    Status(..),
    getStatus,
    getExists,
    getTemporaryDirectory,
    remove,
    removeIfExists,
    removeTreeIfExists,
    move,
    copy,
    resolve,
    Lock,
    withLock,
    acquireLock,
    releaseLock,
    listFilesByExtension,
  )
  where

import GraphDB.Util.Prelude hiding (stripPrefix, last)
import Filesystem.Path.CurrentOS
import Filesystem
import qualified System.Directory as Directory
import qualified Data.List as List
import qualified System.IO.Error as IOError
import qualified System.FileLock as Lock



data Status = File | Directory | NotExists
  deriving (Show, Eq, Ord, Enum)

getStatus :: FilePath -> IO Status
getStatus path = do
  z <- isFile path
  if z
    then return File
    else do
      z <- isDirectory path
      if z
        then return Directory
        else return NotExists

getExists :: FilePath -> IO Bool
getExists path = getStatus path >>= return . (/= NotExists)

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = 
  Directory.getTemporaryDirectory >>= return . decodeString

remove :: FilePath -> IO ()
remove path = do
  status <- getStatus path
  case status of
    File -> removeFile path
    Directory -> removeTree path
    NotExists -> IOError.ioError $ IOError.mkIOError IOError.doesNotExistErrorType "" Nothing (Just $ encodeString path)

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  status <- getStatus path
  case status of
    File -> removeFile path
    Directory -> removeTree path
    NotExists -> return ()

removeTreeIfExists :: FilePath -> IO ()
removeTreeIfExists path = removeTree path `catch` \e -> case e of
  _ | IOError.isDoesNotExistError e -> return ()
    | otherwise -> throwIO e

move :: FilePath -> FilePath -> IO ()
move from to = do
  copy from to
  remove from

copy :: FilePath -> FilePath -> IO ()
copy from to = do
  isDir <- isDirectory from
  if isDir
    then do
      createTree to
      copyDirectory from to
    else do
      createTree $ directory to
      copyFile from to

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory path path' = do
  members <- listDirectory path
  let members' = do
        member <- members
        let relative = 
              fromMaybe (error "Unexpectedly empty member path") $
              last member
        return $ path' <> relative
  sequence_ $ zipWith copy members members'

last :: FilePath -> Maybe FilePath
last p = case splitDirectories p of
  [] -> Nothing
  l -> Just $ List.last l

resolve :: FilePath -> IO FilePath
resolve path = case splitDirectories path of
  h:t | h == "~" -> do
    home <- getHomeDirectory
    return $ mconcat $ home : t
  _ -> return path



type Lock = Lock.FileLock

-- | 
-- Execute an IO action while using a file as an interprocess lock, 
-- thus ensuring that only a single action executes across all processes 
-- of the running system.
-- 
-- If a file exists already it checks whether it is locked on by any running process, 
-- including the current one,
-- and acquires it if it's not.
-- 
-- Releases the lock in case of any failure in executed action or when the action is executed.
withLock :: FilePath -> IO a -> IO a
withLock file io = bracket (acquireLock file) releaseLock (const io)

acquireLock :: FilePath -> IO Lock
acquireLock path = 
  Lock.tryLockFile (encodeString path) Lock.Exclusive >>= \case
    Just lock -> return lock
    _ -> error $ "Lock `" ++ show path ++ "` is already in use"

releaseLock :: Lock -> IO ()
releaseLock = Lock.unlockFile

listFilesByExtension :: FilePath -> Text -> IO [FilePath]
listFilesByExtension dir extension = 
  listDirectory dir >>=
  return . filter (flip hasExtension extension)

