module TPM.GraphDB.Transaction.NodeRef where

import TPM.GraphDB.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



data NodeRef db s = NodeRef Int (IORef (Node db))

getNode :: NodeRef db s -> IO (Node db)
getNode (NodeRef _ ioRef) = readIORef ioRef


