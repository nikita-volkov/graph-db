-- |
-- API of a mutable graph data structure over monomorphic node-value and edge.
module GraphDB.Graph
  (
    Graph,
    new,
    runWrite,
    runRead,
    module GraphDB.Graph.Transaction,
    module GraphDB.Graph.Types,
  )
  where

import GraphDB.Prelude
import GraphDB.Graph.Types
import GraphDB.Graph.Transaction hiding (runWrite, runRead)
import qualified GraphDB.Graph.Dispatcher as Dispatcher; import GraphDB.Graph.Dispatcher (Dispatcher)
import qualified GraphDB.Graph.TypedNode as TypedNode; import GraphDB.Graph.TypedNode (TypedNode)
import qualified GraphDB.Graph.Transaction as Transaction


-- | A mutable graph data structure over nodes of supported types.
data Graph t = Graph {
  root :: TypedNode t (Root t),
  dispatcher :: Dispatcher
}


-- | Initialize a 'Graph' with a value for a root-node.
new :: Root t -> IO (Graph t)
new value = Graph <$> TypedNode.new value <*> Dispatcher.new


-- |
-- Run a write-transaction. 
-- /s/ is a state-thread making the escape of 'NodeRef's from transaction impossible.
runWrite :: Graph t -> (forall s. Transaction.Write t s z) -> IO z
runWrite graph = Dispatcher.runWrite (dispatcher graph) . Transaction.runWrite (root graph)

-- |
-- Run a read-transaction. 
-- /s/ is a state-thread making the escape of 'NodeRef's from transaction impossible.
runRead :: Graph t -> (forall s. Transaction.Read t s z) -> IO z
runRead graph = Dispatcher.runRead (dispatcher graph) . Transaction.runRead (root graph)


instance (GraphTag t) => Serializable IO (Graph t) where
  serialize (Graph root _) = serialize root
  deserialize = Graph <$> deserialize <*> liftIO Dispatcher.new
