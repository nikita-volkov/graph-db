module TPM.GraphDB.Transaction.NodeRefRegistry where

import TPM.Prelude
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Transaction.NodeRef as NodeRef; import TPM.GraphDB.Transaction.NodeRef (NodeRef)
import qualified Data.HashTable.IO as HashTables

-- |
-- Transaction-local registry of references to nodes. These references may not escape
-- the transaction.
-- Inspired by 'ST' monad.
data NodeRefRegistry tag = NodeRefRegistry {
  table :: HashTables.BasicHashTable Int (forall a. Typeable a => Node tag a),
  inc :: Int
}

new :: IO (NodeRefRegistry tag)
new = undefined

newNodeRef :: Node tag a -> (NodeRefRegistry tag) -> IO (NodeRef tag s a)
newNodeRef = undefined

getTargets :: Node.Edge tag a b -> NodeRef tag s a -> (NodeRefRegistry tag) -> IO [NodeRef tag s b]
getTargets edge ref registry = do
  node <- NodeRef.getNode ref
  targetNodes <- Node.getTargets edge node
  traverse (flip newNodeRef registry) targetNodes

-- |
-- For deserialization.
lookup :: Int -> (NodeRefRegistry tag) -> IO (Maybe (NodeRef tag s a))
lookup = undefined

