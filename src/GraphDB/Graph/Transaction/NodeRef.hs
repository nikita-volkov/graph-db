module GraphDB.Graph.Transaction.NodeRef where

import GraphDB.Prelude hiding (Read, Write)
import qualified GraphDB.Graph.Node as Node; import GraphDB.Graph.Node (Node)



-- |
-- A reference to node. 
-- 
-- Cannot escape from transaction.
data NodeRef n e s = NodeRef Int (IORef (Node n e))

new :: Int -> Node n e -> IO (NodeRef n e s)
new index node = NodeRef <$> pure index <*> newIORef node

getNode :: NodeRef n e s -> IO (Node n e)
getNode (NodeRef _ ioRef) = readIORef ioRef

