module TPM.GraphDB.DB where

import TPM.Prelude
import qualified Data.Map as Map
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



data DB = DB {
  root :: Node (),
  dispatcher :: Dispatcher
}

new :: IO DB
new = DB <$> Node.new () <*> Dispatcher.new



