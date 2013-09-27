module TPM.GraphDB.Node where

import TPM.Prelude
import qualified TPM.GraphDB.Node.EdgesTable as EdgesTable



insertEdge :: (Hashable (Edge s t), Eq (Edge s t), Typeable t) 
           => Edge s t -> Node t -> Node s -> IO ()
insertEdge edge target source = do
  EdgesTable.insert (edges source) edge target

deleteEdge :: Edge s t -> Node t -> Node s -> IO ()
deleteEdge = undefined

new :: v -> IO (Node v)
new value = Node <$> newIORef value <*> EdgesTable.new

getValue :: Node v -> IO v
getValue (Node ref _) = readIORef ref

setValue :: v -> Node v -> IO ()
setValue = undefined

getTargets :: Edge s t -> Node s -> IO [Node t]
getTargets edge (Node _ edgesTable) = do
  undefined



data Node v = Node { 
  properties :: IORef v,
  edges :: EdgesTable.EdgesTable v
}

-- |
-- An edge from /source/ value to /target/.
data family Edge source target

type instance EdgesTable.Node source = Node source
type instance EdgesTable.Edge source target = Edge source target


