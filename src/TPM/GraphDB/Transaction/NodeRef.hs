module TPM.GraphDB.Transaction.NodeRef where

import TPM.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



data NodeRef s v = NodeRef Int (IORef (Node v))

getNode :: NodeRef s v -> IO (Node v)
getNode (NodeRef _ ioRef) = readIORef ioRef


