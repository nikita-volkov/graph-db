module GraphDB.Graph.Transaction.NodeRefRegistry where

import GraphDB.Prelude
import qualified GraphDB.Graph.Node as Node; import GraphDB.Graph.Node (Node)
import qualified GraphDB.Graph.Transaction.NodeRef as NodeRef; import GraphDB.Graph.Transaction.NodeRef (NodeRef)
import qualified GraphDB.DIOVector as DIOVector; import GraphDB.DIOVector (DIOVector)

-- |
-- Transaction-local registry of references to nodes. These references may not escape
-- the transaction.
-- Inspired by 'ST' monad.
newtype NodeRefRegistry n e = NodeRefRegistry (DIOVector (Node n e))

new :: IO (NodeRefRegistry n e)
new = NodeRefRegistry <$> DIOVector.new

newNodeRef :: NodeRefRegistry n e -> Node n e -> IO (NodeRef n e s)
newNodeRef (NodeRefRegistry vec) node = do
  -- FIXME: possible race condition
  DIOVector.append vec node
  index <- DIOVector.length vec
  NodeRef.new index node

-- |
-- For deserialization.
lookup :: NodeRefRegistry n e -> Int -> IO (Maybe (NodeRef n e s))
lookup = error "TODO: GraphDB.Graph.Transaction.NodeRefRegistry.lookup"

