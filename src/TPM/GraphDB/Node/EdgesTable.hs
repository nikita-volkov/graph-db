module TPM.GraphDB.Node.EdgesTable where

import TPM.Prelude
import qualified Data.HashTable.IO as Table

type Table k v = Table.BasicHashTable k v



newtype EdgesTable source = 
  EdgesTable 
    ( Table 
        TypeRep 
        ( forall target.  
          Table (Edge source target) [Node target] ) )

type family Edge source target
type family Node value

new :: IO (EdgesTable s)
new = EdgesTable <$> Table.new

lookup 
  :: forall s t. (Hashable (Edge s t), Eq (Edge s t), Typeable t)
  => EdgesTable s -> Edge s t -> IO (Maybe (Node t))
lookup (EdgesTable typeTable) edge = runMaybeT $ do
  edgeTable <- MaybeT $ Table.lookup typeTable (typeOf (undefined :: t))
  MaybeT $ Table.lookup (unsafeCoerce edgeTable :: Table (Edge s t) (Node t)) edge

insert 
  :: forall s t. (Hashable (Edge s t), Eq (Edge s t), Typeable t) 
  => EdgesTable s -> Edge s t -> Node t -> IO ()
insert (EdgesTable typeTable) edge target = do
  edgeTableM <- Table.lookup typeTable typeTableKey
  case edgeTableM of
    Nothing -> do
      edgeTable :: Table (Edge s t) [Node t] <- Table.new
      Table.insert edgeTable edge [target]
      Table.insert typeTable typeTableKey (unsafeCoerce edgeTable)
    Just edgeTable -> do
      targetListM <- Table.lookup edgeTable' edge
      Table.insert edgeTable' edge $ target : fromMaybe [] targetListM
      where
        edgeTable' = unsafeCoerce edgeTable :: Table (Edge s t) [Node t]
  where
    typeTableKey = typeOf (undefined :: t)

foldM :: forall a z. (forall b. z -> (Edge a b, Node b) -> IO z) -> z -> EdgesTable a -> IO z
foldM f z (EdgesTable typeTable) = Table.foldM typeTableFold z typeTable where
  typeTableFold z (tr, edgeTable) = Table.foldM edgeTableFold z edgeTable' where
    edgeTable' = unsafeCoerce edgeTable :: Table (Edge a b') [Node b']
    edgeTableFold z (edge, targetList) = foldr targetListFold (return z) targetList where
      targetListFold target ioZ = ioZ >>= \z -> f z (unsafeCoerce (edge, target))
