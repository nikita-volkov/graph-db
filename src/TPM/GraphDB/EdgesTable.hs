module TPM.GraphDB.EdgesTable where

import TPM.GraphDB.Prelude
import qualified Data.HashTable.IO as Table

type Table k v = Table.BasicHashTable k v



-- |
-- /tag/ makes it possible to have tables with different keys and values
newtype EdgesTable tag = EdgesTable (Table TypeRep (Table (Edge tag) [Node tag]))

type family Edge tag
type family Node tag

new :: IO (EdgesTable tag)
new = EdgesTable <$> Table.new

lookup :: (Hashable (Edge tag), Eq (Edge tag), Typeable (Edge tag)) => Edge tag -> EdgesTable tag -> IO [Node tag]
lookup edge (EdgesTable typeTable) = fmap (fromMaybe []) $ runMaybeT $ do
  edgeTable <- MaybeT $ Table.lookup typeTable (typeOf edge)
  MaybeT $ Table.lookup edgeTable edge

insert :: (Hashable (Edge tag), Eq (Edge tag), Typeable (Edge tag)) => Edge tag -> Node tag -> EdgesTable tag -> IO ()
insert edge target (EdgesTable typeTable) = do
  edgeTableM <- Table.lookup typeTable typeTableKey
  case edgeTableM of
    Nothing -> do
      edgeTable <- Table.new
      Table.insert edgeTable edge [target]
      Table.insert typeTable typeTableKey edgeTable
    Just edgeTable -> do
      targetListM <- Table.lookup edgeTable edge
      Table.insert edgeTable edge $ target : fromMaybe [] targetListM
  where
    typeTableKey = typeOf edge

foldM :: (z -> (Edge tag, Node tag) -> IO z) -> z -> EdgesTable tag -> IO z
foldM f z (EdgesTable typeTable) = Table.foldM typeTableFold z typeTable where
  typeTableFold z (tr, edgeTable) = Table.foldM edgeTableFold z edgeTable where
    edgeTableFold z (edge, targetList) = foldr targetListFold (return z) targetList where
      targetListFold target ioZ = ioZ >>= \z -> f z (edge, target)
