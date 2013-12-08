-- |
-- Utilities for dealing with 'FilePath'.
-- 
module GraphDB.FileSystem
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
    listFilesByExtension,
  )
  where

import GraphDB.Prelude hiding (stripPrefix, last)
import Filesystem.Path.CurrentOS
import Filesystem
import qualified System.Directory as Directory
import qualified Data.List as List
import qualified System.IO.Error as IOError



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

listFilesByExtension :: FilePath -> Text -> IO [FilePath]
listFilesByExtension dir extension = 
  listDirectory dir >>=
  return . filter (flip hasExtension extension)

