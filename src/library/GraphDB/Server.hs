{-# LANGUAGE CPP #-}
-- |
-- Server API.
-- 
-- Communication is done with serializable data. 
module GraphDB.Server 
  (
    ListeningMode(..),
    Request(..),
    Response(..),
    Server,
    start,
    shutdown,
    countActiveClients,
  ) 
  where

import GraphDB.Util.Prelude 
import qualified GraphDB.Util.FileSystem as FS
import qualified Network
import qualified Network.Socket
import qualified Network.Simple.TCP as NetworkSimple
import qualified Pipes.Network.TCP.Safe as PipesNetwork
import qualified Pipes.Prelude as PipesPrelude
import qualified Data.Set as Set


-- | 
-- Defines how to listen for connections.
-- 
data ListeningMode = 
  -- | 
  -- A port to run the server on and a list of acceptable hashes.
  -- Empty list of hashes means a free access.
  -- 
  -- Hashes can be either plain ASCII passwords or an encoding of some data, 
  -- e.g. an MD5 hash of login-password pair.
  Host Int [ByteString] | 
  -- | 
  -- Path to the socket file.
  -- Since sockets are local no password-protection is required.
  -- 
  -- Works only on UNIX systems.
  Socket FilePath


-- | A request from client.
data Request a = 
  -- Authenticate ByteString |
  -- | Request with arbitrary serializable data.
  DataRequest a | 
  -- | Keepalive signal. Resets the connection timeout timer.
  KeepaliveRequest |
  -- | Close the connection.
  DisconnectRequest

deriving instance Generic (Request a)
instance (Serializable m a) => Serializable m (Request a)


-- | A server response.
data Response a =
  -- | An arbitrary data response.
  DataResponse a |
  -- | A command executed successfully (keepalive, disconnect).
  SuccessResponse |
  -- | A failure message.
  FailureResponse Text

deriving instance Generic (Response a)
instance (Serializable m a) => Serializable m (Response a)


data Server request response = Server {
  -- | Wait for all active connections to gracefully close and release all resources.
  shutdown :: IO (),
  -- | Count currently active connections.
  countActiveClients :: IO Int
}


-- | Start a server instance with the provided configuration over a servable data-structure. 
-- 
-- [@Usage@]
-- 
-- > start log timeout mode processRequestData
-- 
-- [@log@] 
-- A logging function.
-- If you want no logging, use @('Control.Monad.void' . return)@, 
-- which is a fancy way of alternatively saying @(\\_ -> return ())@.
-- If you want to output to console use @Data.Text.IO.'Data.Text.IO.putStrLn'@.
-- If you want to somehow reformat the output, you're welcome: 
-- @(Data.Text.IO.'Data.Text.IO.putStrLn' . (\"GraphDB.Server: \" ++))@.
-- 
-- [@timeout@]
-- A connection timeout in ms. Period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
-- 
-- [@mode@]
-- Where to listen for connections, e.g., on a host or a socket.
-- 
-- [@processRequestData@]
-- A function processing the data of data-requests.
-- 
start :: 
  forall i o.
  (Serializable IO i, Serializable IO o) => 
  (Text -> IO ()) -> Int -> ListeningMode -> (i -> IO o) -> IO (Server i o)
start log timeout mode processRequestData = Network.withSocketsDo $ do
  listeningSocket <- Network.listenOn portID
  mainThread <- myThreadId
  listenerThreadVar <- atomically $ newEmptyTMVar
  connectionThreadsVar <- atomically $ newTVar Set.empty
  log "Listening"
  thread <- forkFinally 
    (forever $ acceptConnection mainThread listenerThreadVar connectionThreadsVar listeningSocket)
    (acceptConnectionFinally mainThread listenerThreadVar connectionThreadsVar listeningSocket)
  atomically $ putTMVar listenerThreadVar thread
  return $ Server (shutdown listenerThreadVar connectionThreadsVar)
                  (countActiveClients connectionThreadsVar)
  where
    portID = case mode of
      Host port _ -> Network.PortNumber $ fromIntegral port
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
      Socket file -> Network.UnixSocket $ FS.encodeString file
#else
      error "Socket used on Windows"
#endif
    acceptConnection mainThread listenerThreadVar connectionThreadsVar listeningSocket = do
      (connectionSocket, _) <- Network.Socket.accept listeningSocket
      log "Client connected"
      forkFinally (serveConnection connectionSocket) (serveConnectionFinally connectionSocket)
      where
        serveConnection socket = do
          thread <- myThreadId
          atomically $ modifyTVar connectionThreadsVar $ Set.insert thread
          send (timeout :: Int)
          authenticated <- authenticate
          if authenticated
            then interact
            else do
              log "Invalid authentication attempt"
              return ()
          where
            interact = do
              requestEither <- receive
              case requestEither of
                Left msg -> do
                  log $ "Deserialization failure: " <> msg
                  send (FailureResponse msg :: Response o)
                Right (DataRequest r) -> do
                  responseData <- processRequestData r
                  send (DataResponse responseData :: Response o)
                  interact
                Right KeepaliveRequest -> do
                  send (SuccessResponse :: Response o)
                  interact
                Right DisconnectRequest -> do
                  send (SuccessResponse :: Response o)
                  return ()
            receive :: (Serializable IO z) => IO (Either Text z)
            receive = 
              fmap join $ (fmap . fmap) (maybe (Left "Empty request") Right) $
              runEitherT $ PipesPrelude.head $ 
                PipesNetwork.fromSocketTimeout timeout socket 4096 >-> 
                deserializingPipe
            send :: (Serializable IO z) => z -> IO ()
            send a = runEffect $ 
              serializingProducer a >-> 
              PipesNetwork.toSocketTimeout timeout socket
            authenticate = case mode of
              Host _ validCredentialsList -> do
                receivedCredentials <- receive
                response <- return $ case receivedCredentials of
                  Left m -> Left m
                  Right credentialsMaybe -> 
                    if null validCredentialsList
                      then Right ()
                      else case credentialsMaybe of
                        Nothing -> Left "Empty credentials"
                        Just credentials -> 
                          if elem credentials validCredentialsList
                            then Right ()
                            else Left "Invalid credentials"
                send response
                return $ either (const False) (const True) response 
              _ -> return True
        serveConnectionFinally connectionSocket z = case z of
          Right () -> do
            log "Client disconnected"
            cleanUp
          Left someE 
            | Just ThreadKilled <- fromException someE -> do
                log "Client disconnected: connection thread killed"
                cleanUp
            | Just e <- fromException someE , ioeGetErrorType e == TimeExpired -> do
                log "Client disconnected: interaction timeout"
                cleanUp
            | Just e <- fromException someE , ioeGetErrorType e == ResourceVanished -> do
                log "Client disconnected: resource vanished"
                cleanUp
            | otherwise -> case someE of
                SomeException e -> do
                  log $ "Client disconnected: exception of type " <> (packText $ show $ typeOf e) <> " occurred"
                  cleanUp
                  throwTo mainThread e
          where
            cleanUp = do
              thread <- myThreadId
              Network.sClose connectionSocket
              atomically $ modifyTVar connectionThreadsVar $ Set.delete thread
    acceptConnectionFinally mainThread listenerThreadVar connectionThreadsVar listeningSocket z = case z of
      Right () -> do
        log "Stopped listening"
        cleanUp
      Left someE 
        | Just ThreadKilled <- fromException someE -> do
            log "Stopped listening: listener thread killed"
            cleanUp
        | otherwise -> case someE of
            SomeException e -> do
              log $ "Stopped listening: exception of type " <> (packText $ show $ typeOf e) <> " occurred"
              cleanUp
              throwTo mainThread e
      where
        cleanUp = do
          atomically (readTVar connectionThreadsVar) >>= traverse_ killThread
          atomically $ readTVar connectionThreadsVar >>= \s -> when (not $ Set.null s) retry
          Network.sClose listeningSocket
          case mode of
            Socket path -> FS.removeFile path
            _ -> return ()
          void $ atomically $ tryTakeTMVar listenerThreadVar
    shutdown listenerThreadVar connectionThreadsVar = do
      atomically (tryReadTMVar listenerThreadVar) >>= traverse_ killThread
      atomically $ isEmptyTMVar listenerThreadVar >>= \z -> when (not z) retry
    countActiveClients connectionThreadsVar = do
      atomically $ readTVar connectionThreadsVar >>= return . Set.size
