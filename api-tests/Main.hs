{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import APITests.Prelude hiding (traceIO, traceIOWithTime)
import APITests.Catalogue
import qualified APITests.Prelude as Prelude
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
  runNonpersistentSession $ serve $ runClientSession $ do
    G.write populate
    liftIO . assertEqual (18, 33) =<< G.read (G.getStats :: G.Read s Catalogue t (Int, Int))
  return () :: IO ()
  where
    runNonpersistentSession = G.runNonpersistentSession initRoot where
      initRoot = Catalogue 0
    serve = G.serve (1, lm, to, mc, log) where
      lm = G.ListeningMode_Host 54699 auth where
        auth = const $ return True
      to = 10^6
      mc = 100
      log = traceIOWithTime . ("Server: " <>) . unpackText
    runClientSession = G.runClientSession (1, url) where
      url = G.URL_Host "127.0.0.1" 54699 Nothing

