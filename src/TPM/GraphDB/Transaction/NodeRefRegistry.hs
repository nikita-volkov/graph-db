module TPM.GraphDB.Transaction.NodeRefRegistry where

import TPM.GraphDB.Prelude
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Transaction.NodeRef as NodeRef; import TPM.GraphDB.Transaction.NodeRef (NodeRef)
import qualified TPM.GraphDB.DIOVector as DIOVector; import TPM.GraphDB.DIOVector (DIOVector)

-- |
-- Transaction-local registry of references to nodes. These references may not escape
-- the transaction.
-- Inspired by 'ST' monad.
newtype NodeRefRegistry db = NodeRefRegistry (DIOVector (Node db))

new :: IO (NodeRefRegistry db)
new = NodeRefRegistry <$> DIOVector.new

newNodeRef :: NodeRefRegistry db -> Node db -> IO (NodeRef db s a)
newNodeRef (NodeRefRegistry vec) node = do
  -- FIXME: possible race condition
  DIOVector.append vec node
  index <- DIOVector.length vec
  NodeRef.new index node

-- |
-- For deserialization.
lookup :: NodeRefRegistry db -> Int -> IO (Maybe (NodeRef db s a))
lookup = undefined

