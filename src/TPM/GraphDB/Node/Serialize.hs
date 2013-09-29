module TPM.GraphDB.Node.Serialize where

import TPM.Prelude hiding (put, State, state)
import qualified Data.SafeCopy as SafeCopy
import qualified Data.Serialize as Cereal
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified Data.HashTable.IO as Table
import TPM.GraphDB.Serialization hiding (Table, SafeCopy)

type Table k v = Table.BasicHashTable k v
type SafeCopy = SafeCopy.SafeCopy



-- |
-- A single serializer on a root node.
-- Manages a shared registry of all nodes, thus elluding repeated serialization of same data.
-- 
-- Strategy A:
-- 1. Put a dictionary of all nodes by refs.
-- 
-- Strategy B:
-- 1. Put a node's value.
-- 2. Put all its edges with refs to others nodes.
-- 3. Traverse all referred nodes by going to step 1 with each of them.
-- 
-- FIXME: accumulating 'Cereal.Put' probably is very ineffective.
run :: Node db () -> IO Cereal.Put
run root = do
  state <- undefined
  flip runReaderT state $ do 
    -- processNode root 
    undefined

type Serialize db = ReaderT (State db) (WriterT Cereal.Put IO)
type State db = IORef [(StateNode db, Word64)]
data StateNode db = forall a. (Typeable a, Typeable db) => StateNode (Node db a)

instance Eq (StateNode db) where
  StateNode a == StateNode b = Just a == cast b

-- | Required by 'WriterT'
instance Monoid Cereal.Put where
  mempty = return ()
  mappend a b = a >> b

put :: (Cereal.Serialize a) => a -> Serialize db ()
put a = tell $ Cereal.put a

safePut :: (SafeCopy a) => a -> Serialize db ()
safePut a = tell $ SafeCopy.safePut a

putTerm :: forall db a. (SafeCopy (Term db), IsTerm db a) => a -> Serialize db ()
putTerm a = safePut (toTerm a :: Term db)

putNodeValue :: forall db a. (SafeCopy (Term db), IsTerm db a) => Node db a -> Serialize db ()
putNodeValue node = do
  value <- liftIO $ Node.getValue node
  putTerm value

putNode :: (SafeCopy (Term db), IsTerm db a) => Node db a -> Serialize db ()
putNode node = do
  putNodeValue node
  error "TODO: traverse edges"

putEdge :: (SafeCopy (Term db), IsTerm db (Node.Edge db a b), IsTerm db b, Typeable b, Typeable db) => Node.Edge db a b -> Node db b -> Serialize db ()
putEdge edge node = do
  putTerm edge
  refM <- lookupNodeRef node
  case refM of
    Nothing -> do
      put False
      putNode node
    Just ref -> do
      put True
      put ref

lookupNodeRef :: (Typeable db, Typeable a) => Node db a -> Serialize db (Maybe Word64)
lookupNodeRef node = do
  state <- ask
  liftIO (readIORef state) >>= return . lookup (StateNode node)
