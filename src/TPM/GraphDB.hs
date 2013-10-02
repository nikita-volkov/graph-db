module TPM.GraphDB 
  ( 
    -- * 
    GraphDB,
    new,

    -- * Transactions
    Transaction.Transaction,
    Transaction.Write,
    Transaction.Read,

    -- ** Nodes and Edges
    NodeRef,
    Edge,

    UnionValue,
    IsUnionValueOf(..),

    UnionEdge,
    IsUnionEdgeOf(..),

    -- ** Transaction building blocks
    getRoot,
    newNode,
    getTargets,
    getValue,
    setValue,
    insertEdge,
    deleteEdge,

    -- ** Events
    Event.UnionEvent,
    Event.IsUnionEventOf(..),
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
type GraphDB db = DB.DB db

-- | Initialize a 'DB' with only a single node having a /unit/-value.
new :: (IsUnionValueOf () db) => IO (GraphDB db)
new = DB.new (toUnionValue ())



-- |
-- Properties of edge from /source/ to /target/. E.g.:
-- 
-- @
-- data instance DB.Edge () Artist = UnitToArtistByNameEdge Text | UnitToArtistEdge deriving (Eq, Show, Generic)
-- data instance DB.Edge Artist Genre = ArtistToGenreEdge deriving (Eq, Show, Generic)
-- data instance DB.Edge () Genre = UnitToGenreByNameEdge Text deriving (Eq, Show, Generic)
-- @
-- 
data family Edge source target

-- |
-- A reference to node. 
-- 
-- Cannot escape from transaction.
newtype NodeRef db s value = NodeRef (NodeRef.NodeRef db s)

-- | 
-- A union type for all node values to be used with /db/, required for serialization purposes. E.g.:
-- 
-- @
-- data instance DB.UnionValue Catalogue = UnitValue () | ArtistValue Artist | GenreValue Genre
-- @
-- 
data family UnionValue db
type instance Node.Value db = UnionValue db

-- | 
-- A union type for all edges to be used with /db/, required for serialization purposes. E.g.:
-- 
-- @
-- data instance DB.UnionEdge Catalogue = 
--   UnitToArtistEdge (DB.Edge () Artist) |
--   UnitToGenreEdge (DB.Edge () Genre) |
--   ArtistToGenreEdge (DB.Edge Artist Genre)
-- @
data family UnionEdge db
type instance Node.Edge db = UnionEdge db

-- |
-- Functions for converting a value to and from a union value.
class IsUnionValueOf v db where
  toUnionValue :: v -> UnionValue db
  fromUnionValue :: UnionValue db -> Maybe v

-- |
-- Functions for converting an edge to and from a union value.
class (Hashable (UnionEdge db), Eq (UnionEdge db)) => IsUnionEdgeOf e db where
  toUnionEdge :: e -> UnionEdge db
  fromUnionEdge :: UnionEdge db -> Maybe e



getRoot :: (MonadIO (t db s), Transaction.Transaction t) => t db s (NodeRef db s a)
getRoot = NodeRef `liftM` Transaction.getRoot

newNode :: (IsUnionValueOf a db) => a -> Transaction.Write db s (NodeRef db s a)
newNode value = NodeRef `liftM` Transaction.newNode (toUnionValue value)

getTargets :: (MonadIO (t db s), Transaction.Transaction t, IsUnionEdgeOf (Edge a b) db) => Edge a b -> NodeRef db s a -> t db s [NodeRef db s b]
getTargets edge (NodeRef ref) = liftM (map NodeRef) $ Transaction.getTargets (toUnionEdge edge) ref

getValue :: (MonadIO (t db s), Transaction.Transaction t, IsUnionValueOf a db) => NodeRef db s a -> t db s a
getValue (NodeRef ref) = liftM (fromMaybe bug . fromUnionValue) $ Transaction.getValue ref where
  bug = error "Unexpected value. This is a bug. Please report it."

setValue :: (IsUnionValueOf a db) => a -> NodeRef db s a -> Transaction.Write db s ()
setValue value (NodeRef ref) = Transaction.setValue (toUnionValue value) ref

insertEdge :: (IsUnionEdgeOf (Edge a b) db) => Edge a b -> NodeRef db s a -> NodeRef db s b -> Transaction.Write db s ()
insertEdge edge (NodeRef ref1) (NodeRef ref2) = Transaction.insertEdge (toUnionEdge edge) ref1 ref2

deleteEdge :: (IsUnionEdgeOf (Edge a b) db) => Edge a b -> NodeRef db s a -> NodeRef db s b -> Transaction.Write db s ()
deleteEdge edge (NodeRef ref1) (NodeRef ref2) = Transaction.deleteEdge (toUnionEdge edge) ref1 ref2


