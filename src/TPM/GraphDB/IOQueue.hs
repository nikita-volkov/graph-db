module TPM.GraphDB.IOQueue 
  (
    IOQueue, 
    start, 
    shutdown,
    interrupt,
    enqueue,
  )
  where

import TPM.GraphDB.Prelude


data IOQueue = IOQueue {
  shutdown :: IO (),
  interrupt :: IO (),
  enqueue :: IO () -> IO ()
}

start :: Int -> IO IOQueue
start bufferSize = do
  tasksQueue <- atomically $ newTBQueue bufferSize
  activeVar <- atomically $ newTVar True
  loopThread <- forkIO $ loop tasksQueue activeVar
  return $ IOQueue (shutdown tasksQueue activeVar)
                   (interrupt loopThread)
                   (enqueue tasksQueue activeVar)
  where
    loop tasksQueue activeVar = 
      fetchTask >>= traverse_ (\t -> t >> loop tasksQueue activeVar)
      where
        fetchTask = atomically $ do
          active <- readTVar activeVar
          if active
            then Just <$> readTBQueue tasksQueue
            else tryReadTBQueue tasksQueue
    shutdown tasksQueue activeVar = do
      atomically $ writeTVar activeVar False
      atomically $ isEmptyTBQueue tasksQueue >>= \r -> when (not r) retry
    interrupt loopThread = do
      killThread loopThread
    enqueue tasksQueue activeVar action = do
      thread <- myThreadId
      atomically $ do
        active <- readTVar activeVar
        when active $ do
          writeTBQueue tasksQueue $ handle (handler thread) action
      where
        handler thread e = throwTo thread (e :: SomeException)

