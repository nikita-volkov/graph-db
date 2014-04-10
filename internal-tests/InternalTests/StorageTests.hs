{-# OPTIONS_GHC -F -pgmF htfpp #-}
module InternalTests.StorageTests where

import Test.Framework
import GraphDB.Util.Prelude
import qualified GraphDB.Storage as Storage
import qualified GraphDB.Util.FileSystem as FS



data A = A { ref :: IORef Float }
  deriving (Generic)

instance Serializable IO A

data Event = Increase | Decrease | Multiply Float | Divide Float | Get
  deriving (Show, Ord, Eq, Generic)

instance Serializable m Event

data EventResult = UnitEventResult () | FloatEventResult Float 
  deriving (Show, Ord, Eq, Generic)

instance Serializable m EventResult


initValue = A <$> newIORef 0

applyEvent (A ref) e = case e of
  Increase -> modifyIORef ref succ
  Decrease -> modifyIORef ref pred
  Multiply by -> modifyIORef ref (*by)
  Divide by -> modifyIORef ref (/by)

processRequestData (A ref) event = case event of
  Increase -> modifyIORef ref succ >> readIORef ref >>= return . FloatEventResult
  Decrease -> modifyIORef ref pred >> readIORef ref >>= return . FloatEventResult
  Multiply by -> modifyIORef ref (*by) >> readIORef ref >>= return . FloatEventResult
  Divide by -> modifyIORef ref (/by) >> readIORef ref >>= return . FloatEventResult
  Get -> readIORef ref >>= return . FloatEventResult

dir = "./dist/test/storage"

withStorage :: ((Storage.Storage A Event, A) -> IO a) -> IO a
withStorage = bracket acquire release 
  where
    acquire = Storage.acquireAndLoad initValue applyEvent =<< Storage.pathsFromDirectory dir
    release = Storage.release . fst

cleanUp :: IO ()
cleanUp = FS.removeIfExists dir



test_loadsPastTenAttempts = do 
  cleanUp
  replicateM 11 $ withStorage $ \(s, a) -> do
    Storage.persistEvent s Increase
  withStorage $ \(s, a) -> do
    assertEqual 11 =<< readIORef . ref =<< return a

test_loads = do 
  cleanUp
  withStorage $ \(s, a) -> do
    Storage.persistEvent s Increase
    Storage.persistEvent s Increase
    Storage.persistEvent s $ Multiply 3
    Storage.persistEvent s Decrease
    Storage.persistEvent s Increase
  withStorage $ \(s, a) -> do
    value <- readIORef . ref =<< return a
    assertEqual 6 value

test_loadsAfterMultipleRuns = do 
  cleanUp
  withStorage $ \(s, a) -> do
    Storage.persistEvent s Increase
    Storage.persistEvent s $ Multiply 3
    Storage.persistEvent s Decrease
    Storage.persistEvent s Increase
  withStorage $ \(s, a) -> do
    assertEqual 3 =<< readIORef . ref =<< return a
    Storage.persistEvent s Increase
    Storage.persistEvent s Increase
    Storage.persistEvent s $ Multiply 3
    Storage.persistEvent s Decrease
    Storage.persistEvent s Increase
  withStorage $ \(s, a) -> do
    assertEqual 15 =<< readIORef . ref =<< return a

test_checkpoint = do
  cleanUp
  withStorage $ \(s, a) -> do
    Storage.persistEvent s Increase
    Storage.persistEvent s $ Multiply 3
    Storage.persistEvent s Decrease
    Storage.persistEvent s Increase
  withStorage $ \(s, a) -> do
    Storage.checkpoint s =<< A <$> newIORef 3
  withStorage $ \(s, a) -> assertEqual 3 =<< readIORef . ref =<< return a
  withStorage $ \(s, a) -> do
    Storage.checkpoint s =<< A <$> newIORef 3
    Storage.persistEvent s Increase
    Storage.persistEvent s Increase
    Storage.persistEvent s $ Multiply 3
    Storage.persistEvent s Decrease
    Storage.persistEvent s Increase
  withStorage $ \(s, a) -> assertEqual 15 =<< readIORef . ref =<< return a

test_checkpointWithoutEvents = do 
  cleanUp
  replicateM 11 $ withStorage $ \(s, a) -> do
    applyEvent a Increase
    applyEvent a Increase
    Storage.checkpoint s a
  withStorage $ \(s, a) -> do
    assertEqual 22 =<< readIORef . ref =<< return a

test_checkpointWithEvents = do 
  cleanUp
  replicateM 11 $ withStorage $ \(s, a) -> do
    Storage.persistEvent s Increase
    Storage.persistEvent s Increase
    applyEvent a Increase
    applyEvent a Increase
    Storage.checkpoint s a
  withStorage $ \(s, a) -> do
    assertEqual 22 =<< readIORef . ref =<< return a

test_cleanUp = do
  cleanUp
  withStorage $ \(s, a) -> return ()
  withStorage $ \(s, a) -> Storage.checkpoint s =<< A <$> newIORef 3
  withStorage $ \(s, a) -> Storage.checkpoint s =<< A <$> newIORef 3
  withStorage $ \(s, a) -> return ()
  assertBool =<< FS.getExists (dir <> "4.checkpoint")
  assertBool =<< FS.getExists (dir <> "5.events")
  assertBool =<< FS.getExists (dir <> "6.events")
  assertBool =<< return . not =<< FS.getExists (dir <> "1.events")
  assertBool =<< return . not =<< FS.getExists (dir <> "4.events")
  assertBool =<< FS.getExists (dir <> "archive/1.events")
  assertBool =<< FS.getExists (dir <> "archive/4.events")
  assertBool =<< FS.getExists (dir <> "archive/2.checkpoint")

