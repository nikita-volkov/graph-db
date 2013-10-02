module TPM.GraphDB.DB where

import TPM.GraphDB.Prelude
import qualified Data.Map as Map
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



-- | The Graph Database.
data DB db = DB {
  root :: Node db,
  dispatcher :: Dispatcher
}

-- -- | Initialize a 'DB' with only a single node having a /unit/-value.
-- new :: (IsValue () db) => IO (DB db)
-- new = DB <$> Node.new (toValue ()) <*> Dispatcher.new

-- class IsValue v db where
--   toValue :: v -> Node.Value db
--   fromValue :: Node.Value db -> v


