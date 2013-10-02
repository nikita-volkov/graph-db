module TPM.GraphDB.Node where

import TPM.GraphDB.Prelude
import qualified Data.HashTable.IO as Table



data Node db = Node { 
  value :: IORef (Value db),
  edges :: Table.BasicHashTable (Edge db) [Node db]
}

-- | 
-- A union type for all nodes under a /db/ tag.
-- Used as a dictionary for deserialization.
-- Client specifies a 'SafeCopy' instance for it.
type family Value db 

-- |
-- A union type for all edges under a /db/ tag.
type family Edge db



insertEdge :: (Hashable (Edge db), Eq (Edge db)) => Node db -> Edge db -> Node db -> IO ()
insertEdge (Node _ table) edge target =
  Table.lookup table edge >>=
  return . fromMaybe [] >>=
  return . (target:) >>=
  Table.insert table edge

deleteEdge :: Node db -> Edge db -> Node db -> IO ()
deleteEdge = undefined

new :: Value db -> IO (Node db)
new value = Node <$> newIORef value <*> Table.new

getValue :: Node db -> IO (Value db)
getValue (Node ref _) = readIORef ref

setValue :: Node db -> Value db -> IO ()
setValue = undefined

getTargets :: (Hashable (Edge db), Eq (Edge db)) => Node db -> Edge db -> IO [Node db]
getTargets (Node _ edgesTable) edge = fromMaybe [] <$> Table.lookup edgesTable edge

foldEdgesM :: Node db -> z -> (z -> (Edge db, Node db) -> IO z) -> IO z
foldEdgesM node f z = undefined



instance Eq (Node db) where
  a == b = value a == value b


