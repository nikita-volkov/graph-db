module TPM.GraphDB.Transaction.NodeRef where

import TPM.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



data NodeRef db s a = NodeRef Int (IORef (Node db a))

getNode :: NodeRef db s a -> IO (Node db a)
getNode (NodeRef _ ioRef) = readIORef ioRef


