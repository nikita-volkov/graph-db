{-# LANGUAGE CPP #-}
module GraphDB.Client 
  (
    Client,
    ServerURL(..),
    ClientException(..),
    connect,
    disconnect,
    interrupt,
    request,
  )
  where

import GraphDB.Util.Prelude 
import qualified GraphDB.Server as Server; import GraphDB.Server (Request(..))
import qualified GraphDB.Util.IOQueue as IOQueue; import GraphDB.Util.IOQueue (IOQueue)
import qualified GraphDB.Util.FileSystem as FS
import qualified Network
import qualified Network.Simple.TCP as NetworkSimple
import qualified Pipes.Network.TCP as PipesNetwork
import qualified Pipes.ByteString as PipesByteString
import qualified Pipes.Prelude as PipesPrelude
import qualified System.IO


data Client request response = Client {
  request :: request -> IO response,
  disconnect :: IO (),
  interrupt :: IO ()
}

-- |
-- Location of server.
data ServerURL =
  -- | Path to the socket-file.
  Socket FilePath |
  -- | Host name, port and password.
  Host Text Int (Maybe ByteString)

data ClientException =
  AuthenticationFailure Text |
  DeserializationFailure Text |
  -- |
  -- Server sent a failure text message. E.g., it couldn't deserialize data.
  -- May happen in case of versions mismatch with server.
  FailureResponse Text |
  -- EmptyResponse |
  InvalidResponse |
  -- |
  -- Can't establish a connection to server. It is offline or otherwise unreachable.
  CantConnect
  deriving (Typeable, Show)

instance Exception ClientException

connect :: (Serializable IO i, Serializable IO o) => ServerURL -> IO (Client i o)
connect url = Network.withSocketsDo $ do
  requestsQueue <- IOQueue.start 100
  keepaliveThreadVar <- newEmptyMVar
  let
    connect = handle handler $ case url of
      Socket file -> 
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
        Network.connectTo "" (Network.UnixSocket $ FS.encodeString file)
#else
        error "Socket used on Windows"
#endif
      Host name port _ -> 
        Network.connectTo (unpackText name) (Network.PortNumber $ fromIntegral port)
      where
        handler e = case e of
          _ | isDoesNotExistError e -> throwIO CantConnect
          _ -> throwIO e
  socketHandle <- connect
  let
    receive :: Serializable IO z => IO z
    receive = do
      r <- runEitherT $ PipesPrelude.head $ 
        PipesByteString.fromHandle socketHandle >-> deserializingPipe
      case r of
        Left m -> throwIO $ DeserializationFailure m
        Right Nothing -> throwIO $ CantConnect
        Right (Just r) -> return r
    send :: Serializable IO z => z -> IO ()
    send a = 
      runEffect $ serializingProducer a >-> PipesByteString.toHandle socketHandle
    authenticate = do
      case url of
        Host _ _ credentials -> do
          send credentials
          response <- receive
          case response of
            Left m -> throwIO $ AuthenticationFailure m
            Right () -> return ()
        _ -> return ()
  serverTimeout <- receive :: IO Int
  let
    request a = handleRequest $ do
      responseVar <- newEmptyMVar
      IOQueue.enqueue requestsQueue $ do
        send a
        putMVar responseVar =<< receive
      takeMVar responseVar 
      where
        handleRequest = handle $ \e -> case e of
          _ | ioeGetErrorType e == ResourceVanished -> do
            cleanUp
            throwIO CantConnect
          _ -> do
            cleanUp
            throwIO e
    resetKeepaliveTimer = do
      tryTakeMVar keepaliveThreadVar >>= traverse_ killThread
      parentThread <- myThreadId
      let
        handleForkedThread = handle $ \e -> if
          | Just ThreadKilled <- fromException e -> return ()
          | otherwise -> throwTo parentThread e
      thread <- forkIO $ handleForkedThread $ do
        threadDelay timeout
        mask_ $ do
          request KeepaliveRequest >>= \r -> case r of
            Server.FailureResponse m -> throwIO $ FailureResponse m
            Server.SuccessResponse -> return ()
            _ -> throwIO $ InvalidResponse
          void $ takeMVar keepaliveThreadVar
          resetKeepaliveTimer
      putMVar keepaliveThreadVar thread
      where
        timeout = if linear > 10 * 10^6 then linear else percent
          where
            linear = serverTimeout - (3 * 10^6)
            percent = round (fromIntegral serverTimeout * 0.5)
    dataRequest a = do
      resetKeepaliveTimer
      response <- request $ DataRequest a
      case response of
        Server.FailureResponse m -> throwIO $ FailureResponse m
        Server.DataResponse r -> return r
        _ -> throwIO $ InvalidResponse
    disconnect = do
      request DisconnectRequest >>= \r -> case r of
        Server.FailureResponse m -> throwIO $ FailureResponse m
        Server.SuccessResponse -> return ()
        _ -> throwIO $ InvalidResponse
      IOQueue.shutdown requestsQueue
      cleanUp
    interrupt = cleanUp
    cleanUp = do
      tryTakeMVar keepaliveThreadVar >>= traverse_ killThread
      IOQueue.interrupt requestsQueue
      handle (const $ return () :: SomeException -> IO ()) $ hClose socketHandle
  authenticate
  resetKeepaliveTimer
  return $ Client dataRequest disconnect interrupt
