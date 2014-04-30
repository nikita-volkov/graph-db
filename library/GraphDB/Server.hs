module GraphDB.Server where

import GraphDB.Util.Prelude
import qualified Remotion.Server as R
import qualified GraphDB.Util.DIOVector as V
import qualified GraphDB.Protocol as P
import qualified GraphDB.Action as A
import qualified GraphDB.Graph as G


type Action n s = A.Action n (G.Value s) (G.Index s)

type Command s = Maybe (P.Action s)

type Comm s = (Chan (Command s), MVar (P.Response s))

runCommandProcessor :: (MonadIO m) => Comm s -> Action n s m ()
runCommandProcessor (commandsChan, responseVar) = do
  refs <- liftIO $ V.new
  let 
    newRef = liftIO . V.append refs
    resolveRef = liftIO . V.unsafeLookup refs
    respond = liftIO . putMVar responseVar
    loop = do 
      com <- liftIO $ readChan commandsChan
      case com of
        Nothing -> return ()
        Just ra -> do
          case ra of
            P.NewNode v -> do
              n <- A.newNode v
              r <- newRef n
              respond $ P.Node r
            P.GetValue sr -> do
              sn <- resolveRef sr
              r <- A.getValue sn
              respond $ P.Value r
            P.SetValue r v -> do
              n <- resolveRef r
              A.setValue n v
              respond $ P.Unit
            P.GetRoot -> do
              n <- A.getRoot
              r <- newRef n
              respond $ P.Node r
            P.GetTargets r i -> do
              n <- resolveRef r
              nl <- A.getTargets n i
              rl <- mapM newRef nl
              respond $ P.NodeList rl
            P.AddTarget s t -> do
              sn <- resolveRef s
              tn <- resolveRef t
              A.addTarget sn tn
              respond $ P.Unit
            P.RemoveTarget s t -> do
              sn <- resolveRef s
              tn <- resolveRef t
              A.removeTarget sn tn
              respond $ P.Unit
            P.Remove r -> do
              n <- resolveRef r
              A.remove n
              respond $ P.Unit
            P.GetStats -> do
              r <- A.getStats
              respond $ P.Stats r
          loop
    in loop

processRequest :: Chan (Bool, Comm s) -> IORef (Maybe (Comm s)) -> P.Request s -> IO (P.Response s)
processRequest transactionsChan stateRef req = do
  -- Initialize the state if needed and determine whether it's the final request.
  state <- readIORef stateRef
  case req of
    P.Start write -> do
      when (isJust state) $ $bug "Transaction state is already filled"
      state <- (,) <$> newChan <*> newEmptyMVar
      writeChan transactionsChan $ (write, state)
      writeIORef stateRef $ Just $ state
      return $ P.Unit
    P.Finish -> do
      case state of
        Nothing -> $bug "No transaction state"
        Just (commandsChan, _) -> do
          writeChan commandsChan Nothing
          writeIORef stateRef $ Nothing
          return $ P.Unit
    P.Action act -> do
      case state of
        Nothing -> $bug "No transaction state"
        Just (commandsChan, responseVar) -> do
          writeChan commandsChan $ Just act
          takeMVar responseVar
 
-- * Serve
-------------------------

-- |
-- A monad transformer for running the server.
-- 
-- Can only be executed inside a 'Session' using 'serve'.
newtype Serve m r = 
  Serve (R.Server m r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadTransControl Serve where
  newtype StT Serve r = ServeStT (StT R.Server r)
  liftWith runInInner = do
    Serve $ liftWith $ \runServer -> runInInner $ \(Serve s) -> liftM ServeStT $ runServer s
  restoreT inner = do
    Serve $ do
      ServeStT r <- lift $ inner
      restoreT $ return $ r

instance (MonadBase IO m) => MonadBase IO (Serve m) where
  liftBase = Serve . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (Serve m) where
  newtype StM (Serve m) a = ServeStM { unServeStM :: ComposeSt Serve m a }
  liftBaseWith = defaultLiftBaseWith ServeStM
  restoreM = defaultRestoreM unServeStM

-- |
-- Block the calling thread until the server stops (which should never happen).
block :: MonadIO m => Serve m ()
block = Serve $ R.wait
