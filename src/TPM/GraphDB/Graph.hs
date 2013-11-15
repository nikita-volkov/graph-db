-- |
-- API of a mutable graph data structure over monomorphic node-value and edge.
module TPM.GraphDB.Graph
  (
    Graph,
    new,

    -- * Transactions
    Transaction.Write,
    Transaction.Read,
    Transaction.Reads,
    runWrite,
    runRead,
    Transaction.NodeRef,

    -- ** Transaction building blocks
    Transaction.getRoot,
    Transaction.newNode,
    Transaction.getTargets,
    Transaction.getValue,
    Transaction.setValue,
    Transaction.insertEdge,
    Transaction.deleteEdge,

  ) where

import TPM.GraphDB.Prelude
import qualified TPM.GraphDB.Graph.Dispatcher as Dispatcher; import TPM.GraphDB.Graph.Dispatcher (Dispatcher)
import qualified TPM.GraphDB.Graph.Node as Node; import TPM.GraphDB.Graph.Node (Node)
import qualified TPM.GraphDB.Graph.Transaction as Transaction
import qualified TPM.GraphDB.Graph.Transaction.NodeRef as Transaction



-- | A mutable graph data structure over node properties and edge properties.
data Graph n e = Graph {
  root :: Node n e,
  dispatcher :: Dispatcher
}

instance Eq (Graph n e) where
  a == b = root a == root b

instance (Serializable n IO, Serializable e IO, Hashable e, Eq e) => Serializable (Graph n e) IO where
  serialize = serialize . root
  deserialize = Graph <$> deserialize <*> (liftIO $ Dispatcher.new)


-- | Initialize a 'Graph' with a value for a root-node.
new :: n -> IO (Graph n e)
new value = Graph <$> Node.new value <*> Dispatcher.new


-- |
-- Run a write-transaction. 
-- /s/ is a state-thread making the escape of 'NodeRef's from transaction impossible.
runWrite :: Graph n e -> (forall s. Transaction.Write n e s r) -> IO r
runWrite graph = Dispatcher.runWrite (dispatcher graph) . Transaction.runWrite (root graph)

-- |
-- Run a read-transaction. 
-- /s/ is a state-thread making the escape of 'NodeRef's from transaction impossible.
runRead :: Graph n e -> (forall s. Transaction.Read n e s r) -> IO r
runRead graph = Dispatcher.runRead (dispatcher graph) . Transaction.runRead (root graph)

