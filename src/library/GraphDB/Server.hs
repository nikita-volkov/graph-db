module GraphDB.Server
  (ServerMode(..), Server, shutdownServer, startServer)
  where

import GraphDB.Util.Prelude hiding (Read, log)
import qualified AcidIO.Server as Server
import qualified GraphDB.Engine as Engine

--------------------------------------------------------------------------------

-- | 
-- The settings of server.
data ServerMode = 
  -- | 
  -- A port to run the server on and a list of acceptable passwords.
  -- Empty list of passwords means a free access.
  -- 
  ServerMode_Host Int [ByteString] | 
  -- | 
  -- Path to the socket file.
  -- Since sockets are local no password-protection is required.
  -- 
  ServerMode_Socket FilePath

data Server t = Server {
  shutdownServer :: IO ()
}

startServer :: 
  (Engine.Tag t, Serializable IO (Engine.UnionEvent t), Serializable IO (Engine.UnionEventResult t)) =>
  Engine.Engine t -> ServerMode -> IO (Server t)
startServer engine serverMode = do
  acidServer <- Server.start (void . return) (5 * 60 * 10^6) acidServerMode processRequest
  let
    shutdown = Server.shutdown acidServer
  return $ Server shutdown
  where
    acidServerMode = case serverMode of
      ServerMode_Socket path -> Server.Socket path
      ServerMode_Host port passwords -> Server.Host port passwords
    processRequest unionEvent = Engine.runUnionEvent engine unionEvent
