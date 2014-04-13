module GraphDB.Util.IOQueue 
  (
    IOQueue, 
    start, 
    perform,
    performAsync,
    finish,
  )
  where

import GraphDB.Util.Prelude
import qualified Control.Concurrent.Async as Async

data IOQueue = IOQueue {
  perform :: forall r. IO r -> IO r,
  performAsync :: IO () -> IO (),
  finish :: IO ()
}

start :: Int -> IO IOQueue
start size = do
  (tasksVar, activeVar) <- atomically $ (,) <$> newTBQueue size <*> newTVar True
  let loop = do
        task <- atomically $ do
          tryReadTBQueue tasksVar >>= \case
            Nothing -> readTVar activeVar >>= \case
              True -> retry
              False -> return Nothing
            Just task -> return $ Just task
        traverse_ (\t -> t >> loop) task
  loopAsync <- Async.async $ loop
  let performAsync task = do
        atomically $ do
          readTVar activeVar >>= \case
            True -> writeTBQueue tasksVar task
            False -> return ()
      perform :: IO r -> IO r
      perform task = do
        resultVar <- newEmptyMVar
        performAsync $ do
          result <- task
          putMVar resultVar result
        takeMVar resultVar
      finish = do
        atomically $ writeTVar activeVar False
        Async.wait loopAsync
        return ()
  return $ IOQueue perform performAsync finish
