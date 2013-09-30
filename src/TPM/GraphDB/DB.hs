module TPM.GraphDB.DB where

import TPM.GraphDB.Prelude
import qualified Data.Map as Map
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



data DB tag = DB {
  root :: Node tag (),
  dispatcher :: Dispatcher
}

new :: IO (DB tag)
new = DB <$> Node.new () <*> Dispatcher.new



