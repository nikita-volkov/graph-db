module TPM.GraphDB.Node.EdgesTable where

import TPM.Prelude
import qualified Data.HashTable.IO as Table

type Table k v = Table.BasicHashTable k v



newtype EdgesTable tag source = 
  EdgesTable 
    ( Table 
        TypeRep 
        ( forall target.  
          Table (Edge tag source target) [Node tag target] ) )

type family Edge tag source target
type family Node tag value

new :: IO (EdgesTable tag s)
new = EdgesTable <$> Table.new

lookup 
  :: forall tag s t. (Hashable (Edge tag s t), Eq (Edge tag s t), Typeable t)
  => EdgesTable tag s -> Edge tag s t -> IO (Maybe (Node tag t))
lookup (EdgesTable typeTable) edge = runMaybeT $ do
  edgeTable <- MaybeT $ Table.lookup typeTable (typeOf (undefined :: t))
  MaybeT $ Table.lookup (unsafeCoerce edgeTable :: Table (Edge tag s t) (Node tag t)) edge

insert 
  :: forall tag s t. (Hashable (Edge tag s t), Eq (Edge tag s t), Typeable t) 
  => EdgesTable tag s -> Edge tag s t -> Node tag t -> IO ()
insert (EdgesTable typeTable) edge target = do
  edgeTableM <- Table.lookup typeTable typeTableKey
  case edgeTableM of
    Nothing -> do
      edgeTable :: Table (Edge tag s t) [Node tag t] <- Table.new
      Table.insert edgeTable edge [target]
      Table.insert typeTable typeTableKey (unsafeCoerce edgeTable)
    Just edgeTable -> do
      targetListM <- Table.lookup edgeTable' edge
      Table.insert edgeTable' edge $ target : fromMaybe [] targetListM
      where
        edgeTable' = unsafeCoerce edgeTable :: Table (Edge tag s t) [Node tag t]
  where
    typeTableKey = typeOf (undefined :: t)

foldM :: forall tag a z. 
         (forall b. z -> (Edge tag a b, Node tag b) -> IO z) -> z -> EdgesTable tag a -> IO z
foldM f z (EdgesTable typeTable) = Table.foldM typeTableFold z typeTable where
  typeTableFold z (tr, edgeTable) = Table.foldM edgeTableFold z edgeTable' where
    edgeTable' = unsafeCoerce edgeTable :: Table (Edge tag a b') [Node tag b']
    edgeTableFold z (edge, targetList) = foldr targetListFold (return z) targetList where
      targetListFold target ioZ = ioZ >>= \z -> f z (unsafeCoerce (edge, target))
