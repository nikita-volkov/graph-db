module TPM.GraphDB.Node where

import TPM.GraphDB.Prelude
import qualified TPM.GraphDB.EdgesTable as EdgesTable
import TPM.GraphDB.Serialization
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal



insertEdge :: (Hashable (Edge db a b), Eq (Edge db a b), Typeable (Edge db a b), IsTerm b db, Typeable db, Typeable b, IsTerm (Edge db a b) db) 
           => Edge db a b -> Node db b -> Node db a -> IO ()
insertEdge edge target source = do
  EdgesTable.insert (AnyEdge edge) (AnyNode target) (edges source)

deleteEdge :: Edge db a b -> Node db b -> Node db a -> IO ()
deleteEdge = undefined

new :: a -> IO (Node db a)
new value = Node <$> newIORef value <*> EdgesTable.new

getValue :: Node db a -> IO a
getValue (Node ref _) = readIORef ref

setValue :: a -> Node db a -> IO ()
setValue = undefined

getTargets :: (Hashable (Edge db a b), Eq (Edge db a b), Typeable (Edge db a b), Typeable db, IsTerm (Edge db a b) db) 
           => Edge db a b -> Node db a -> IO [Node db b]
getTargets edge (Node _ edgesTable) = do
  etNodes <- EdgesTable.lookup (AnyEdge edge) edgesTable  
  return $ do
    AnyNode node <- etNodes
    return $ unsafeCoerce node



data Node db a = Node { 
  properties :: IORef a,
  edges :: EdgesTable.EdgesTable db
}

instance Eq (Node db a) where
  a == b = properties a == properties b
deriving instance Typeable2 Node

-- |
-- An edge from /source/ value to /target/ tagged with /db/.
data family Edge db a b
deriving instance Typeable3 Edge



data AnyNode db = forall a. (IsTerm a db, Typeable a, Typeable db) => AnyNode (Node db a)
type instance EdgesTable.Node db = AnyNode db
instance Eq (AnyNode db) where
  AnyNode a == AnyNode b = Just a == cast b

data AnyEdge db =
  forall a b. 
  (Hashable (Edge db a b), Typeable (Edge db a b), Eq (Edge db a b), IsTerm (Edge db a b) db) => 
  AnyEdge (Edge db a b)
type instance EdgesTable.Edge db = AnyEdge db
deriving instance Typeable1 AnyEdge
instance Hashable (AnyEdge db) where
  hashWithSalt salt (AnyEdge edge) = hashWithSalt (succ salt) edge
instance Eq (AnyEdge db) where
  AnyEdge a == AnyEdge b = Just a == cast b


