{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.StorageTests where

import Test.Framework
import GraphDB.Util.Prelude
import qualified GraphDB.Storage as Storage
import qualified GraphDB.Util.FileSystem as FS
import qualified HTFTestSuite.A as A



dir = "./dist/test/acid"

withStorage :: ((Storage.Storage A.A A.Event, A.A) -> IO a) -> IO a
withStorage = bracket acquire release 
  where
    acquire = Storage.acquireAndLoad A.initValue A.applyEvent =<< Storage.pathsFromDirectory dir
    release = Storage.release . fst

cleanUp :: IO ()
cleanUp = FS.removeIfExists dir



test_loadsPastTenAttempts = do 
  cleanUp
  replicateM 11 $ withStorage $ \(s, a) -> do
    Storage.persistEvent s A.Increase
  withStorage $ \(s, a) -> do
    assertEqual 11 =<< readIORef . A.ref =<< return a

test_loads = do 
  cleanUp
  withStorage $ \(s, a) -> do
    Storage.persistEvent s A.Increase
    Storage.persistEvent s A.Increase
    Storage.persistEvent s $ A.Multiply 3
    Storage.persistEvent s A.Decrease
    Storage.persistEvent s A.Increase
  withStorage $ \(s, a) -> do
    value <- readIORef . A.ref =<< return a
    assertEqual 6 value

test_loadsAfterMultipleRuns = do 
  cleanUp
  withStorage $ \(s, a) -> do
    Storage.persistEvent s A.Increase
    Storage.persistEvent s $ A.Multiply 3
    Storage.persistEvent s A.Decrease
    Storage.persistEvent s A.Increase
  withStorage $ \(s, a) -> do
    assertEqual 3 =<< readIORef . A.ref =<< return a
    Storage.persistEvent s A.Increase
    Storage.persistEvent s A.Increase
    Storage.persistEvent s $ A.Multiply 3
    Storage.persistEvent s A.Decrease
    Storage.persistEvent s A.Increase
  withStorage $ \(s, a) -> do
    assertEqual 15 =<< readIORef . A.ref =<< return a

test_checkpoint = do
  cleanUp
  withStorage $ \(s, a) -> do
    Storage.persistEvent s A.Increase
    Storage.persistEvent s $ A.Multiply 3
    Storage.persistEvent s A.Decrease
    Storage.persistEvent s A.Increase
  withStorage $ \(s, a) -> do
    Storage.checkpoint s =<< A.A <$> newIORef 3
  withStorage $ \(s, a) -> assertEqual 3 =<< readIORef . A.ref =<< return a
  withStorage $ \(s, a) -> do
    Storage.checkpoint s =<< A.A <$> newIORef 3
    Storage.persistEvent s A.Increase
    Storage.persistEvent s A.Increase
    Storage.persistEvent s $ A.Multiply 3
    Storage.persistEvent s A.Decrease
    Storage.persistEvent s A.Increase
  withStorage $ \(s, a) -> assertEqual 15 =<< readIORef . A.ref =<< return a

test_checkpointWithoutEvents = do 
  cleanUp
  replicateM 11 $ withStorage $ \(s, a) -> do
    A.applyEvent a A.Increase
    A.applyEvent a A.Increase
    Storage.checkpoint s a
  withStorage $ \(s, a) -> do
    assertEqual 22 =<< readIORef . A.ref =<< return a

test_checkpointWithEvents = do 
  cleanUp
  replicateM 11 $ withStorage $ \(s, a) -> do
    Storage.persistEvent s A.Increase
    Storage.persistEvent s A.Increase
    A.applyEvent a A.Increase
    A.applyEvent a A.Increase
    Storage.checkpoint s a
  withStorage $ \(s, a) -> do
    assertEqual 22 =<< readIORef . A.ref =<< return a

test_cleanUp = do
  cleanUp
  withStorage $ \(s, a) -> return ()
  withStorage $ \(s, a) -> Storage.checkpoint s =<< A.A <$> newIORef 3
  withStorage $ \(s, a) -> Storage.checkpoint s =<< A.A <$> newIORef 3
  withStorage $ \(s, a) -> return ()
  assertBool =<< FS.getExists (dir <> "4.checkpoint")
  assertBool =<< FS.getExists (dir <> "5.events")
  assertBool =<< FS.getExists (dir <> "6.events")
  assertBool =<< return . not =<< FS.getExists (dir <> "1.events")
  assertBool =<< return . not =<< FS.getExists (dir <> "4.events")
  assertBool =<< FS.getExists (dir <> "archive/1.events")
  assertBool =<< FS.getExists (dir <> "archive/4.events")
  assertBool =<< FS.getExists (dir <> "archive/2.checkpoint")

