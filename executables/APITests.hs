{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.QuickCheck.Monadic
import APITests.Prelude hiding (traceIO, traceIOWithTime, assert)
import qualified APITests.Prelude as Prelude
import qualified APITests.Util.FileSystem as FS
import qualified GraphDB as G
import APITests.Catalogue


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
  assertEqual [Identified 1 (Artist "The Beatles")] =<< do
    runNonpersistentSession $ do
      serve $ runClientSession $ do
        G.write $ insertArtist $ Artist "The Beatles"
      G.read $ 
        G.getRoot >>= 
        flip G.getTargets (Catalogue_Artist_Name "The Beatles") >>=
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
