{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.QuickCheck.Monadic
import APITests.Prelude hiding (traceIO, traceIOWithTime, assert)
import APITests.Catalogue
import qualified APITests.Prelude as Prelude
import qualified APITests.Util.FileSystem as FS
import qualified GraphDB as G


main = htfMain $ htf_thisModulesTests


-- Debugging
-------------------------
-- The following functions get enabled during debugging.

debugging = True
prefix = id
traceIO = if debugging 
  then Prelude.traceIO . prefix 
  else const $ return ()
traceIOWithTime = if debugging 
  then Prelude.traceIOWithTime . prefix 
  else const $ return ()

-- Tests
-------------------------

test_clientServer = do
  assertEqual [Identified (fromIntegral 1) (Artist "The Beatles")] =<< do
    runNonpersistentSession $ do
      serve $ runClientSession $ do
        G.write $ insertArtist $ Artist "The Beatles"
      G.read $ 
        G.getRoot >>= 
        flip G.getTargetsByIndex (Catalogue_Artist_Name "The Beatles") >>=
        mapM G.getValue
  where
    runNonpersistentSession = G.runNonpersistentSession initRoot
    serve = G.serve (1, lm, to, mc, log) where
      lm = G.ListeningMode_Host 54699 auth where
        auth = const $ return True
      to = 10^6
      mc = 100
      log = traceIOWithTime . ("Server: " <>) . unpackText
    runClientSession = G.runClientSession (1, url) where
      url = G.URL_Host "127.0.0.1" 54699 Nothing

test_addingANodeTwiceThrowsNoError = do
  G.runNonpersistentSession initRoot $ do
    G.write $ do
      node <- G.newNode $ Identified (UID 1) (Artist "A")
      root <- G.getRoot
      G.addTarget root node
      G.addTarget root node
  return () :: IO ()

test_removingANodeAffectsStats = do
  G.runNonpersistentSession initRoot $ do
    liftIO . assertEqual (1, 0) =<< G.read G.getStats
    uid <- G.write $ insertArtist $ Artist "A"
    liftIO . assertEqual (2, 1) =<< G.read G.getStats
    G.write $ do
      root <- G.getRoot
      nodes <- G.getTargetsByIndex root (Catalogue_Artist_UID uid)
      mapM_ G.remove nodes
    liftIO . assertEqual (1, 0) =<< G.read G.getStats
  return () :: IO ()

prop_shutdownDBRestoresToTheSameState = monadicIO $ do
  run $ initDir
  updates <- pick $ do
    amount <- choose (0, 100)
    replicateM amount (arbitrary :: Gen (Update G.PersistentSession t))
  stats <- 
    fmap (either (error . show) id) $
    run $ runPersistentSession $ do
      forM_ updates $ \(Update write) -> G.write $ unsafeCoerce $ write
      G.read G.getStats
  stats' <- 
    fmap (either (error . show) id) $ 
    run $ runPersistentSession $ G.read G.getStats
  run $ traceIOWithTime $ 
    "Stats 1: " <> show stats <> ", stats 2: " <> show stats' <> ", " <>
    "updates: " <> show (length updates)
  assert $ stats == stats'

-- prop_interruptedDBRestoresToTheSameState :: Property
-- prop_interruptedDBRestoresToTheSameState = undefined


-- * Setup
-------------------------

runPersistentSession ::   
  (MonadBaseControl IO m, MonadIO m) =>
  G.PersistentSession Catalogue m r -> m (Either G.PersistenceFailure r)
runPersistentSession = G.runPersistentSession (initRoot, dir, buffering) where
  buffering = 100

runNonpersistentSession :: 
  (MonadIO m) => G.NonpersistentSession Catalogue m r -> m r
runNonpersistentSession = G.runNonpersistentSession initRoot

serve socket = G.serve (1, lm, to, mc, log) where
  lm = if socket 
    then G.ListeningMode_Socket socketPath
    else G.ListeningMode_Host 54699 auth 
    where
      auth = const $ return True
  to = 10^6
  mc = 100
  log = const $ return ()

runClientSession socket = G.runClientSession (1, url) where
  url = if socket
    then G.URL_Socket socketPath
    else G.URL_Host "127.0.0.1" 54699 Nothing

initDir :: IO ()
initDir = do
  FS.removeIfExists dir
  FS.createTree dir

socketPath = dir <> ".socket"
dir = "./dist/test/graph-db"
