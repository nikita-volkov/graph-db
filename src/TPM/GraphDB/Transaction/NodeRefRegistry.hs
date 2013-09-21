module TPM.GraphDB.Transaction.NodeRefRegistry where

import TPM.Prelude
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Transaction.NodeRef as NodeRef; import TPM.GraphDB.Transaction.NodeRef (NodeRef)
import qualified Data.HashTable.IO as HashTables

-- |
-- Transaction-local registry of references to nodes. These references may not escape
-- the transaction.
-- Inspired by 'ST' monad.
data NodeRefRegistry = NodeRefRegistry {
  table :: HashTables.BasicHashTable Int (forall a. Typeable a => Node a),
  inc :: Int
}

new :: IO NodeRefRegistry
new = undefined

newNodeRef :: Node a -> NodeRefRegistry -> IO (NodeRef s a)
newNodeRef = undefined

getTargets :: Node.Edge a b -> NodeRef s a -> NodeRefRegistry -> IO [NodeRef s b]
getTargets edge ref registry = do
  node <- NodeRef.getNode ref
  targetNodes <- Node.getTargets edge node
  traverse (flip newNodeRef registry) targetNodes

-- |
-- For deserialization.
lookup :: Int -> NodeRefRegistry -> IO (Maybe (NodeRef s a))
lookup = undefined

