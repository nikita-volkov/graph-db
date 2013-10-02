module TPM.GraphDB.Transaction.NodeRef where

import TPM.GraphDB.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)



-- |
-- A reference to node. 
-- 
-- Cannot escape from transaction.
data NodeRef db s a = NodeRef Int (IORef (Node db))

new :: Int -> Node db -> IO (NodeRef db s a)
new index node = NodeRef <$> pure index <*> newIORef node

getNode :: NodeRef db s a -> IO (Node db)
getNode (NodeRef _ ioRef) = readIORef ioRef

