module TPM.GraphDB.Transaction.NodeRefRegistry where

import TPM.GraphDB.Prelude
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Transaction.NodeRef as NodeRef; import TPM.GraphDB.Transaction.NodeRef (NodeRef)
import qualified Data.HashTable.IO as HashTables
import qualified TPM.GraphDB.Serialization as Serialization; import TPM.GraphDB.Serialization (IsTerm)

-- |
-- Transaction-local registry of references to nodes. These references may not escape
-- the transaction.
-- Inspired by 'ST' monad.
data NodeRefRegistry db = NodeRefRegistry {
  table :: HashTables.BasicHashTable Int (forall a. Typeable a => Node db a),
  inc :: Int
}

new :: IO (NodeRefRegistry db)
new = undefined

newNodeRef :: Node db a -> (NodeRefRegistry db) -> IO (NodeRef db s a)
newNodeRef = undefined

getTargets :: (Hashable e, Typeable e, Eq e, e ~ Node.Edge db a b, Typeable db, IsTerm (Node.Edge db a b) db) 
           => Node.Edge db a b -> NodeRef db s a -> (NodeRefRegistry db) -> IO [NodeRef db s b]
getTargets edge ref registry = do
  node <- NodeRef.getNode ref
  targetNodes <- Node.getTargets edge node
  traverse (flip newNodeRef registry) targetNodes

-- |
-- For deserialization.
lookup :: Int -> (NodeRefRegistry db) -> IO (Maybe (NodeRef db s a))
lookup = undefined

