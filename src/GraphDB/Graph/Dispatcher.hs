module GraphDB.Graph.Dispatcher (Dispatcher, new, runWrite, runRead) where

import GraphDB.Prelude
import qualified Control.Concurrent.FairRWLock as RWLock



-- | 
-- Controls the concurrency with the following rules:
-- 
-- * It ensures that when a \"write\"-action executes, no other action executes
-- concurrently.
-- 
-- * It ensures that no \"write\"-actions get executed while a \"read\"-action 
-- executes.
-- 
data Dispatcher = Dispatcher RWLock.RWLock

new :: IO Dispatcher
new = Dispatcher <$> RWLock.new

-- |
runWrite :: Dispatcher -> IO a -> IO a
runWrite (Dispatcher lock) io = RWLock.withWrite lock io

-- |
runRead :: Dispatcher -> IO a -> IO a
runRead (Dispatcher lock) io = RWLock.withRead lock io



