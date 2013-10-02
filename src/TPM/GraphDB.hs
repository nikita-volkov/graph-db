module TPM.GraphDB 
  ( 
    -- * 
    GraphDB,
    new,

    NodeRef,
    Edge,

    ValueUnion,
    EdgeUnion,

    IsValue(..),
    IsEdge(..),

    -- * Transactions
    Transaction.Transaction,
    Transaction.Write,
    Transaction.Read,
    
    -- ** Transaction building blocks
    getRoot,
    newNode,
    getTargets,
    getValue,
    setValue,
    insertEdge,
    deleteEdge,

    -- ** Events
    Event.Event(..),
    Event.run,
  ) where

import TPM.GraphDB.Prelude
import qualified TPM.GraphDB.DB as DB
import qualified TPM.GraphDB.Transaction as Transaction
import qualified TPM.GraphDB.Transaction.Event as Event
import qualified TPM.GraphDB.Transaction.NodeRef as NodeRef
import qualified TPM.GraphDB.Node as Node
import qualified TPM.GraphDB.Dispatcher as Dispatcher



-- | 
-- The Graph Database.
newtype GraphDB db = GraphDB (DB.DB db)



-- |
-- An edge from /source/ to /target/.
data family Edge source target

-- |
-- A reference to node. 
-- 
-- Cannot escape from transaction.
newtype NodeRef db s value = NodeRef (NodeRef.NodeRef db s)

-- | 
-- A union type for all node values to be used with /db/. E.g.:
-- 
-- @
-- data instance DB.ValueUnion Catalogue = UnitValue () | ArtistValue Artist | GenreValue Genre
-- @
-- 
data family ValueUnion db
type instance Node.Value db = ValueUnion db

-- | 
-- A union type for all edges to be used with /db/. E.g.:
-- 
-- @
-- data instance DB.EdgeUnion Catalogue = 
--   UnitToArtistEdge (DB.Edge () Artist) |
--   UnitToGenreEdge (DB.Edge () Genre) |
--   ArtistToGenreEdge (DB.Edge Artist Genre)
-- @
data family EdgeUnion db
type instance Node.Edge db = EdgeUnion db


-- |
-- Functions for converting a value to and from a union value.
class IsValue v db where
  toValueUnion :: v -> ValueUnion db
  fromValueUnion :: ValueUnion db -> v

-- |
-- Functions for converting an edge to and from a union value.
class (Hashable (EdgeUnion db), Eq (EdgeUnion db)) => IsEdge e db where
  toEdgeUnion :: e -> EdgeUnion db
  fromEdgeUnion :: EdgeUnion db -> e



-- | Initialize a 'DB' with only a single node having a /unit/-value.
new :: (IsValue () db) => IO (GraphDB db)
new = GraphDB <$> (DB.DB <$> Node.new (toValueUnion ()) <*> Dispatcher.new)

getRoot :: (MonadIO (t db s), Transaction.Transaction t) => t db s (NodeRef db s a)
getRoot = NodeRef `liftM` Transaction.getRoot

newNode :: (IsValue a db) => a -> Transaction.Write db s (NodeRef db s a)
newNode value = NodeRef `liftM` Transaction.newNode (toValueUnion value)

getTargets :: (MonadIO (t db s), Transaction.Transaction t, IsEdge (Edge a b) db) => Edge a b -> NodeRef db s a -> t db s [NodeRef db s b]
getTargets edge (NodeRef ref) = liftM (map NodeRef) $ Transaction.getTargets (toEdgeUnion edge) ref

getValue :: (MonadIO (t db s), Transaction.Transaction t, IsValue a db) => NodeRef db s a -> t db s a
getValue (NodeRef ref) = liftM fromValueUnion $ Transaction.getValue ref

setValue :: (IsValue a db) => a -> NodeRef db s a -> Transaction.Write db s ()
setValue value (NodeRef ref) = Transaction.setValue (toValueUnion value) ref

insertEdge :: (IsEdge (Edge a b) db) => Edge a b -> NodeRef db s a -> NodeRef db s b -> Transaction.Write db s ()
insertEdge edge (NodeRef ref1) (NodeRef ref2) = Transaction.insertEdge (toEdgeUnion edge) ref1 ref2

deleteEdge :: (IsEdge (Edge a b) db) => Edge a b -> NodeRef db s a -> NodeRef db s b -> Transaction.Write db s ()
deleteEdge edge (NodeRef ref1) (NodeRef ref2) = Transaction.deleteEdge (toEdgeUnion edge) ref1 ref2

