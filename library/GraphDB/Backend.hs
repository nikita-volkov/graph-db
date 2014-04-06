module GraphDB.Backend where

import GraphDB.Util.Prelude
import qualified GraphDB.FreeTransaction.Action as Action

-- |
-- A session backend.
class Backend b where
  -- |
  -- A monad transformer, 
  -- which can execute transactions and run a server over some backend.
  data Session b u m r
  -- |
  -- Backend-specific settings of a session.
  data SessionSettings b 
  -- |
  -- A backend-specific session result.
  type SessionResult b
  runAction :: (Monad m) => Bool -> Action.Action b u r -> Session b u m r 
  -- |
  -- Run a session on a backend with the provided settings.
  runSession :: SessionSettings b -> Session b u m r -> m (SessionResult b r)
