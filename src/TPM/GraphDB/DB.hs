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

new :: Node.Value db -> IO (DB db)
new value = DB <$> Node.new value <*> Dispatcher.new
