module TPM.GraphDB.Node where

import TPM.Prelude
import qualified TPM.GraphDB.EdgesTable as EdgesTable
import qualified TPM.GraphDB.Serialization as Serialization



insertEdge :: (Hashable (Edge a b), Eq (Edge a b), Typeable (Edge a b)) 
           => Edge a b -> Node b -> Node a -> IO ()
insertEdge edge target source = do
  EdgesTable.insert (EdgesTableEdge edge) (EdgesTableNode target) (edges source)

deleteEdge :: Edge a b -> Node b -> Node a -> IO ()
deleteEdge = undefined

new :: a -> IO (Node a)
new value = Node <$> newIORef value <*> EdgesTable.new

getValue :: Node a -> IO a
getValue (Node ref _) = readIORef ref

setValue :: a -> Node a -> IO ()
setValue = undefined

getTargets :: (Hashable (Edge a b), Eq (Edge a b), Typeable (Edge a b)) 
           => Edge a b -> Node a -> IO [Node b]
getTargets edge (Node _ edgesTable) = do
  etNodes <- EdgesTable.lookup (EdgesTableEdge edge) edgesTable  
  return $ do
    EdgesTableNode node <- etNodes
    return $ unsafeCoerce node



data Node a = Node { 
  properties :: IORef a,
  edges :: EdgesTable.EdgesTable
}

instance Eq (Node a) where
  a == b = properties a == properties b
deriving instance Typeable1 Node

-- |
-- An edge from /source/ value to /target/ tagged with /db/.
data family Edge a b

data instance EdgesTable.Node = 
  forall a. 
  EdgesTableNode (Node a)
data instance EdgesTable.Edge = 
  forall a b. 
  (Hashable (Edge a b), Typeable (Edge a b), Eq (Edge a b)) => 
  EdgesTableEdge (Edge a b)

deriving instance Typeable EdgesTable.Edge
instance Hashable EdgesTable.Edge where
  hashWithSalt salt (EdgesTableEdge edge) = hashWithSalt (succ salt) edge
instance Eq EdgesTable.Edge where
  EdgesTableEdge a == EdgesTableEdge b = Just a == cast b



