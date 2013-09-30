module TPM.GraphDB.EdgesTable where

import TPM.Prelude
import qualified Data.HashTable.IO as Table

type Table k v = Table.BasicHashTable k v



newtype EdgesTable = EdgesTable (Table TypeRep (Table Edge [Node]))

data family Edge 
data family Node 

new :: IO EdgesTable
new = EdgesTable <$> Table.new

lookup :: (Hashable Edge, Eq Edge, Typeable Edge) => Edge -> EdgesTable -> IO [Node]
lookup edge (EdgesTable typeTable) = fmap (fromMaybe []) $ runMaybeT $ do
  edgeTable <- MaybeT $ Table.lookup typeTable (typeOf edge)
  MaybeT $ Table.lookup edgeTable edge

insert :: (Hashable Edge, Eq Edge, Typeable Edge) => Edge -> Node -> EdgesTable -> IO ()
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

foldM :: (z -> (Edge, Node) -> IO z) -> z -> EdgesTable -> IO z
foldM f z (EdgesTable typeTable) = Table.foldM typeTableFold z typeTable where
  typeTableFold z (tr, edgeTable) = Table.foldM edgeTableFold z edgeTable where
    edgeTableFold z (edge, targetList) = foldr targetListFold (return z) targetList where
      targetListFold target ioZ = ioZ >>= \z -> f z (edge, target)
