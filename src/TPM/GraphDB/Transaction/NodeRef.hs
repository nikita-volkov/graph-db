module TPM.GraphDB.Transaction.NodeRef where

import TPM.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



data NodeRef t s a = NodeRef Int (IORef (Node t a))

getNode :: NodeRef t s a -> IO (Node t a)
getNode (NodeRef _ ioRef) = readIORef ioRef


