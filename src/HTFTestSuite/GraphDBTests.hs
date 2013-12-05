{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TupleSections #-}
module HTFTestSuite.GraphDBTests where

import GraphDB.Prelude hiding (assert)
import Test.Framework
import Test.QuickCheck.Monadic
import HTFTestSuite.CatalogueDB hiding (arbitrary)
import qualified GraphDB as G
import qualified GraphDB.FileSystem as FS
import qualified GHC.DataSize
import qualified Data.Time
import qualified System.Locale



prop_shutdownDBRestoresToTheSameState :: Property
prop_shutdownDBRestoresToTheSameState = monadicIO $ do
  updates <- pick (arbitrary :: Gen [Update ()])
  dir <- arbitraryStorageDir

  (run $ FS.getExists dir) >>= pre . not

  (stats, stats', size, size') <- run $ flip finally (FS.remove dir) $ do
    db <- startPersistedDB dir
    forM_ updates $ \update -> runUpdate update db
    stats <- G.runEvent db GetStats
    size <- GHC.DataSize.recursiveSize db
    G.shutdownEngine db

    db' <- startPersistedDB dir
    stats' <- G.runEvent db' GetStats
    size' <- GHC.DataSize.recursiveSize db'
    G.shutdownEngine db'

    return (stats, stats', size, size')

  -- traceM $ "Comparison: " ++ show stats ++ " / " ++ show stats' ++ ",\t" ++ show size ++ " / " ++ show size'
  pre ((case stats of (n, _) -> n) > 1)
  assert $ stats == stats'
  -- assert $ size == size'


prop_unshutdownDBRestoresToTheSameState :: Property
prop_unshutdownDBRestoresToTheSameState = monadicIO $ do
  updates <- pick (arbitrary :: Gen [Update ()])
  dir <- arbitraryStorageDir

  (run $ FS.getExists dir) >>= pre . not

  (stats, stats', size, size') <- run $ flip finally (FS.remove dir) $ do
    db <- startPersistedDB dir
    forM_ updates $ \update -> runUpdate update db
    stats <- G.runEvent db GetStats
    size <- GHC.DataSize.recursiveSize db
    G.shutdownEngine' db

    db' <- startPersistedDB dir
    stats' <- G.runEvent db' GetStats
    size' <- GHC.DataSize.recursiveSize db'
    G.shutdownEngine' db'

    return (stats, stats', size, size')

  -- traceM $ "Comparison: " ++ show stats ++ " / " ++ show stats' ++ ",\t" ++ show size ++ " / " ++ show size'
  pre ((case stats of (n, _) -> n) > 1)
  assert $ stats == stats'
  -- assert $ size == size'


test_startupShutdown'1 = do
  FS.removeIfExists dir

  db <- startPersistedDB dir
  populate db
  stats <- G.runEvent db GetStats
  size <- GHC.DataSize.recursiveSize db
  allReleases <- G.runEvent db GetAllReleases
  artistsOfAllReleases <- 
    fmap join $ forM allReleases $ \r -> do
      let Release uid _ _ _ = r
      G.runEvent db (GetArtistsByReleaseUID uid)
  G.shutdownEngine' db

  db' <- startPersistedDB dir
  stats' <- G.runEvent db' GetStats
  size' <- GHC.DataSize.recursiveSize db'
  allReleases' <- G.runEvent db' GetAllReleases
  artistsOfAllReleases' <- 
    fmap join $ forM allReleases $ \r -> do
      let Release uid _ _ _ = r
      G.runEvent db' (GetArtistsByReleaseUID uid)
  G.shutdownEngine' db'

  assertEqualVerbose "length allReleases == 1" 1 (length allReleases)
  assertEqualVerbose "length artistsOfAllReleases == 1" 1 (length artistsOfAllReleases)
  assertEqualVerbose 
    "length allReleases' == length allReleases" 
    (length allReleases) 
    (length allReleases')
  assertEqualVerbose 
    "length artistsOfAllReleases' == length artistsOfAllReleases" 
    (length artistsOfAllReleases) 
    (length artistsOfAllReleases')
  assertEqualVerbose "stats' == stats" stats stats'
  where
    dir = storageRoot <> "test_startupShutdown'1" <> "storage"

test_startupShutdown1 = do
  FS.removeIfExists dir

  db <- startPersistedDB dir
  populate db
  stats <- G.runEvent db GetStats
  size <- GHC.DataSize.recursiveSize db
  allReleases <- G.runEvent db GetAllReleases
  artistsOfAllReleases <- 
    fmap join $ forM allReleases $ \r -> do
      let Release uid _ _ _ = r
      G.runEvent db (GetArtistsByReleaseUID uid)
  G.shutdownEngine db

  db' <- startPersistedDB dir
  stats' <- G.runEvent db' GetStats
  size' <- GHC.DataSize.recursiveSize db'
  allReleases' <- G.runEvent db' GetAllReleases
  artistsOfAllReleases' <- 
    fmap join $ forM allReleases $ \r -> do
      let Release uid _ _ _ = r
      G.runEvent db' (GetArtistsByReleaseUID uid)
  G.shutdownEngine db'

  assertEqualVerbose "length allReleases == 1" 1 (length allReleases)
  assertEqualVerbose "length artistsOfAllReleases == 1" 1 (length artistsOfAllReleases)
  assertEqualVerbose 
    "length allReleases' == length allReleases" 
    (length allReleases) 
    (length allReleases')
  assertEqualVerbose 
    "length artistsOfAllReleases' == length artistsOfAllReleases" 
    (length artistsOfAllReleases) 
    (length artistsOfAllReleases')
  assertEqualVerbose "stats' == stats" stats stats'
  where
    dir = storageRoot <> "test_startupShutdown1" <> "storage"



startUnpersistedDB :: IO (G.Engine Catalogue)
startUnpersistedDB = G.startEngine (Catalogue 0) =<< (return . G.Mode_Local) Nothing

startPersistedDB :: FilePath -> IO (G.Engine Catalogue)
startPersistedDB dir = G.startEngine initRoot =<< getLocalPersistedMode
  where
    initRoot = Catalogue 0
    getLocalPersistedMode = return . G.Mode_Local . Just . (100,) =<< G.pathsFromDirectory dir

arbitraryStorageDir :: PropertyM IO FilePath
arbitraryStorageDir = do
  subdir <- FS.decodeString <$> pick (listOf $ elements ['a'..'z'])
  return $ storageRoot <> subdir

storageRoot = "./dist/test/GraphDBTests/"

populate db = do
  metallica <- do
    uid <- G.runEvent db $ GenerateNewUID
    return $ Artist uid "Metallica"
  G.runEvent db $ LinkCatalogueToArtist metallica
  load <- do
    uid <- G.runEvent db $ GenerateNewUID
    return $ Release uid "Load" StudioAlbum (dayFromString "1996-07-01")
  G.runEvent db $ LinkCatalogueToRelease load
  G.runEvent db $ LinkReleaseToArtist load (TitleArtist True) metallica
  G.runEvent db $ LinkArtistToRelease metallica load
  do
    recording <- Recording <$> (G.runEvent db GenerateNewUID) <*> pure (5*60+4) <*> pure StudioRecording
    -- song <- Song <$> pure "Ain't My Bitch"
    G.runEvent db $ LinkCatalogueToRecording recording
    G.runEvent db $ LinkReleaseToRecording load (Track 1) recording
    G.runEvent db $ LinkRecordingToArtist recording (TitleArtist True) metallica
    G.runEvent db $ LinkArtistToRecording metallica recording
  do
    recording <- Recording <$> (G.runEvent db GenerateNewUID) <*> pure (4*60+28) <*> pure StudioRecording
    -- song <- Song <$> pure "Until It Sleeps"
    G.runEvent db $ LinkCatalogueToRecording recording
    G.runEvent db $ LinkReleaseToRecording load (Track 4) recording
    G.runEvent db $ LinkRecordingToArtist recording (TitleArtist True) metallica
    G.runEvent db $ LinkArtistToRecording metallica recording
  where
    dayFromString = Data.Time.readTime System.Locale.defaultTimeLocale "%Y-%m-%d"

