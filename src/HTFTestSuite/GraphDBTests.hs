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
  run $ prepareEnvironment
  updates <- pick (arbitrary :: Gen [Update ()])

  (stats, stats', size, size') <- run $ do
    db <- startPersistedDB
    forM_ updates $ \update -> runUpdate update db
    stats <- G.runEvent db GetStats
    size <- GHC.DataSize.recursiveSize db
    G.shutdownEngine db

    db' <- startPersistedDB
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
  run $ prepareEnvironment
  updates <- pick (arbitrary :: Gen [Update ()])

  (stats, stats', size, size') <- run $ do
    db <- startPersistedDB
    forM_ updates $ \update -> runUpdate update db
    stats <- G.runEvent db GetStats
    size <- GHC.DataSize.recursiveSize db
    G.shutdownEngine' db

    db' <- startPersistedDB
    stats' <- G.runEvent db' GetStats
    size' <- GHC.DataSize.recursiveSize db'
    G.shutdownEngine' db'

    return (stats, stats', size, size')

  -- traceM $ "Comparison: " ++ show stats ++ " / " ++ show stats' ++ ",\t" ++ show size ++ " / " ++ show size'
  pre ((case stats of (n, _) -> n) > 1)
  assert $ stats == stats'
  -- assert $ size == size'


test_startupShutdown'1 = do
  prepareEnvironment

  db <- startPersistedDB
  populate db
  stats <- G.runEvent db GetStats
  size <- GHC.DataSize.recursiveSize db
  allReleases <- G.runEvent db GetAllReleases
  artistsOfAllReleases <- 
    fmap join $ forM allReleases $ \r -> do
      let Release uid _ _ _ = r
      G.runEvent db (GetArtistsByReleaseUID uid)
  G.shutdownEngine' db

  db' <- startPersistedDB
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

test_startupShutdown1 = do
  prepareEnvironment

  db <- startPersistedDB
  populate db
  stats <- G.runEvent db GetStats
  size <- GHC.DataSize.recursiveSize db
  allReleases <- G.runEvent db GetAllReleases
  artistsOfAllReleases <- 
    fmap join $ forM allReleases $ \r -> do
      let Release uid _ _ _ = r
      G.runEvent db (GetArtistsByReleaseUID uid)
  G.shutdownEngine db

  db' <- startPersistedDB
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

test_setValue = do
  prepareEnvironment
  db <- startPersistedDB

  uid <- G.runEvent db $ GenerateNewUID
  let artist = Artist uid "A"
  G.runEvent db $ LinkCatalogueToArtist artist

  G.runEvent db (GetArtistByUID uid) >>=
    assertEqualVerbose "Value is reachable by actual indexes" (Just artist)

  uid' <- G.runEvent db $ GenerateNewUID
  let artist' = Artist uid' "B"
  G.runEvent db $ SetArtistByUID artist' uid

  G.runEvent db (GetArtistByUID uid) >>= 
    assertEqualVerbose "Value becomes unreachable by old indexes" Nothing

  G.runEvent db (GetArtistByUID uid') >>= \vm -> do
    assertBoolVerbose "Value is reachable by new indexes" (isJust vm)
    forM_ vm $ assertEqualVerbose "Value is updated" artist'
  


startUnpersistedDB :: IO (G.Engine Catalogue)
startUnpersistedDB = G.startEngine (Catalogue 0) =<< (return . G.Mode_Local) Nothing

startPersistedDB :: IO (G.Engine Catalogue)
startPersistedDB = G.startEngine initRoot =<< getLocalPersistedMode
  where
    initRoot = Catalogue 0
    getLocalPersistedMode = return . G.Mode_Local . Just . (100,) =<< G.pathsFromDirectory storageDir

prepareEnvironment = do
  FS.removeIfExists storageDir

storageDir = "./dist/test/GraphDBTests/storage/"

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

