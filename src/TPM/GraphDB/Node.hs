module TPM.GraphDB.Node where

import TPM.Prelude
import qualified TPM.GraphDB.Node.EdgesTable as EdgesTable



insertEdge :: (Hashable (Edge tag a b), Eq (Edge tag a b), Typeable b) 
           => Edge tag a b -> Node tag b -> Node tag a -> IO ()
insertEdge edge target source = do
  EdgesTable.insert (edges source) edge target

deleteEdge :: Edge tag a b -> Node tag b -> Node tag a -> IO ()
deleteEdge = undefined

new :: a -> IO (Node tag a)
new value = Node <$> newIORef value <*> EdgesTable.new

getValue :: Node tag a -> IO a
getValue (Node ref _) = readIORef ref

setValue :: a -> Node tag a -> IO ()
setValue = undefined

getTargets :: Edge tag a b -> Node tag a -> IO [Node tag b]
getTargets edge (Node _ edgesTable) = do
  undefined



data Node tag a = Node { 
  properties :: IORef a,
  edges :: EdgesTable.EdgesTable tag a
}

-- |
-- An edge from /source/ value to /target/.
data family Edge tag source target

type instance EdgesTable.Node tag source = Node tag source
type instance EdgesTable.Edge tag source target = Edge tag source target


