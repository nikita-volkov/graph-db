module TPM.GraphDB.Node where

import TPM.Prelude
import qualified TPM.GraphDB.Node.EdgesTable as EdgesTable



insertEdge :: (Hashable (Edge db a b), Eq (Edge db a b), Typeable b) 
           => Edge db a b -> Node db b -> Node db a -> IO ()
insertEdge edge target source = do
  EdgesTable.insert (edges source) edge target

deleteEdge :: Edge db a b -> Node db b -> Node db a -> IO ()
deleteEdge = undefined

new :: a -> IO (Node db a)
new value = Node <$> newIORef value <*> EdgesTable.new

getValue :: Node db a -> IO a
getValue (Node ref _) = readIORef ref

setValue :: a -> Node db a -> IO ()
setValue = undefined

getTargets :: Edge db a b -> Node db a -> IO [Node db b]
getTargets edge (Node _ edgesTable) = do
  undefined



data Node db a = Node { 
  properties :: IORef a,
  edges :: EdgesTable.EdgesTable db a
}

instance Eq (Node db a) where
  a == b = properties a == properties b
deriving instance Typeable2 Node

-- |
-- An edge from /source/ value to /target/ tagged with /db/.
data family Edge db source target

type instance EdgesTable.Node db source = Node db source
type instance EdgesTable.Edge db source target = Edge db source target


