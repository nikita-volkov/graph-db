{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.ServerAndClientTests where

import Test.Framework
import GraphDB.Util.Prelude hiding (timeout)
import qualified HTFTestSuite.A as A
import qualified GraphDB.Server as Server; import GraphDB.Server (Server)
import qualified GraphDB.Client as Client; import GraphDB.Client (Client)
import Control.Concurrent.Async


dir = "./dist/test/"
timeout = 500 * 10 ^ 3

startOnHost :: IO (Server A.Event A.EventResult)
startOnHost = 
  Server.start (void . return) timeout (Server.Host 45039 ["p1", "p2"]) =<< 
  return . A.processRequestData =<<
  A.A <$> newIORef 0

startOnSocket :: IO (Server A.Event A.EventResult)
startOnSocket = 
  Server.start (void . return) timeout (Server.Socket $ dir <> ".socket") =<< 
  return . A.processRequestData =<<
  A.A <$> newIORef 0

connectToHost :: Maybe ByteString -> IO (Client A.Event A.EventResult)
connectToHost credentials = Client.connect (Client.Host "127.0.0.1" 45039 credentials)

connectToSocket :: IO (Client A.Event A.EventResult)
connectToSocket = Client.connect (Client.Socket $ dir <> ".socket")


test_socketConnection = do
  server <- startOnSocket
  client <- connectToSocket
  assertEqual (A.FloatEventResult 1) =<< Client.request client (A.Increase)
  Client.disconnect client
  Server.shutdown server

test_startSameServerTwice = do
  Server.shutdown =<< startOnSocket
  Server.shutdown =<< startOnSocket

test_hostConnection = do
  server <- startOnHost
  client <- connectToHost (Just "p2")
  assertEqual (A.FloatEventResult 1) =<< Client.request client (A.Increase)
  Client.disconnect client
  Server.shutdown server

test_unauthenticated = bracket startOnHost Server.shutdown $ \server -> do
  assertThrowsIO (connectToHost $ Just "p0") $ \e -> case e of
    Client.AuthenticationFailure "Invalid credentials" -> True
    _ -> False
  assertThrowsIO (connectToHost Nothing) $ \e -> case e of
    Client.AuthenticationFailure "Empty credentials" -> True
    _ -> False

test_connectToAnOfflineServer = do
  server <- startOnHost
  Server.shutdown server
  assertThrowsIO (connectToHost $ Just "p1") $ \e -> case e of
    Client.CantConnect -> True
    _ -> False

test_requestAnOfflineServer = do
  server <- startOnHost
  client <- connectToHost $ Just "p1"
  Server.shutdown server
  assertThrowsIO (Client.request client A.Increase) $ \e -> case e of
    Client.CantConnect -> True
    _ -> False

test_disconnectFromAnOfflineServer = do
  server <- startOnHost
  client <- connectToHost $ Just "p1"
  Server.shutdown server
  assertThrowsIO (Client.disconnect client) $ \e -> case e of
    Client.CantConnect -> True
    _ -> False

test_invalidClientRequests = 
  unitTestPending "The server seems to successfully deserialize all kinds of data"

test_clientConnectionTimeout = bracket startOnHost Server.shutdown $ \server -> do
  assertEqual 0 =<< Server.countActiveClients server
  client <- connectToHost $ Just "p1"
  assertEqual 1 =<< Server.countActiveClients server
  Client.interrupt client
  threadDelay $ 1 * 10^6
  assertEqual 0 =<< Server.countActiveClients server

test_keepalive = bracket startOnHost Server.shutdown $ \server -> do
  assertEqual 0 =<< Server.countActiveClients server
  client <- connectToHost $ Just "p1"
  assertEqual 1 =<< Server.countActiveClients server
  threadDelay $ 1 * 10^6
  assertEqual 1 =<< Server.countActiveClients server

test_clientDisconnect = bracket startOnHost Server.shutdown $ \server -> do
  assertEqual 0 =<< Server.countActiveClients server
  client <- connectToHost $ Just "p1"
  assertEqual 1 =<< Server.countActiveClients server
  Client.disconnect client
  threadDelay $ 100 * 10^3 -- give it 100ms to realize
  assertEqual 0 =<< Server.countActiveClients server

test_multipleClients = bracket startOnHost Server.shutdown $ \server -> do
  void $ mapConcurrently id $ replicate 9 $ do
    client <- connectToHost (Just "p1")
    Client.request client A.Increase
    Client.disconnect client
  client <- connectToHost (Just "p1")
  assertEqual (A.FloatEventResult 9) =<< Client.request client A.Get
  Client.disconnect client

test_concurrentRequestsFromSingleClient = bracket startOnHost Server.shutdown $ \server -> do
  client <- connectToHost (Just "p1")
  void $ mapConcurrently id $ replicate 9 $ do
    Client.request client A.Increase
    Client.request client A.Increase
    Client.request client A.Decrease
  assertEqual (A.FloatEventResult 9) =<< Client.request client A.Get
  Client.disconnect client



