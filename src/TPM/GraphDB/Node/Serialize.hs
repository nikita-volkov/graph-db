module TPM.GraphDB.Node.Serialize where

import TPM.GraphDB.Prelude hiding (put, State, state)
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal
import qualified TPM.GraphDB.EdgesTable as EdgesTable
import TPM.GraphDB.Serialization hiding (Table, SafeCopy)
import TPM.GraphDB.Node as Node



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
-- FIXME: accumulating 'Cereal.Put' probably is very inefficient.
run :: (SafeCopy (Term db), IsTerm a db, Typeable db, Typeable a) => Node db a -> IO Cereal.Put
run root = do
  state <- newIORef ([], 0)
  execWriterT $ flip runReaderT state $ putNode $ AnyNode root

type Serialize db = ReaderT (State db) (WriterT Cereal.Put IO)
-- | A list of serialized nodes and its length.
type State db = IORef ([AnyNode db], Int)

put :: (Cereal.Serialize a) => a -> Serialize db ()
put a = tell $ Cereal.put a

safePut :: (SafeCopy a) => a -> Serialize db ()
safePut a = tell $ SafeCopy.safePut a

putTerm :: forall db a. (SafeCopy (Term db), IsTerm a db) => a -> Serialize db ()
putTerm a = safePut (toTerm a :: Term db)

putNode :: (SafeCopy (Term db)) => AnyNode db -> Serialize db ()
putNode anyNode@(AnyNode node) = putValue >> putEdges >> updateState where
  putValue = do
    value <- liftIO $ Node.getValue node
    putTerm value
  putEdges = do
    (edgesCount, putEdges) <- liftIO $ EdgesTable.foldM fold (0 :: Int, return ()) $ Node.edges node
    put edgesCount
    putEdges
    where
      fold (count, put) (edge, node) = return $ (succ count, putEdgeAndTarget edge node)
  updateState = do
    state <- ask
    liftIO $ modifyIORef state $ \(list, length) -> (anyNode : list, succ length)

putEdgeAndTarget :: (SafeCopy (Term db)) => AnyEdge db -> AnyNode db -> Serialize db ()
putEdgeAndTarget (AnyEdge edge) anyNode = do
  putTerm edge
  refM <- lookupNodeIndex anyNode
  case refM of
    Nothing -> do
      put False
      putNode anyNode
    Just ref -> do
      put True
      put ref

lookupNodeIndex :: AnyNode db -> Serialize db (Maybe Int)
lookupNodeIndex anyNode = do
  state <- ask
  -- liftIO (readIORef state) >>= return . lookup anyNode
  error "FIXME: implement"
