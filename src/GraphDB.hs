module GraphDB 
  (
    startEngine,
    shutdownEngine,
    shutdownEngine',
    startServer,
    shutdownServer,
    module GraphDB.Engine,
    -- * Template Haskell 
    module GraphDB.Macros,
  ) 
  where

import GraphDB.Prelude
import GraphDB.Engine hiding (start, shutdown)
import GraphDB.Server hiding (start, shutdown)
import GraphDB.Macros (generateBoilerplate)
import qualified GraphDB.Engine as Engine
import qualified GraphDB.Server as Server

startEngine = Engine.start
shutdownEngine = Engine.shutdown
shutdownEngine' = Engine.shutdown'

startServer = Server.start
shutdownServer = Server.shutdown
