module TPM.GraphDB.Node.Deserialize where

import TPM.GraphDB.Prelude hiding (get, State, state)
import TPM.GraphDB.Serialization
import TPM.GraphDB.Node as Node
import qualified TPM.GraphDB.EdgesTable as EdgesTable
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Cereal
import qualified Data.HashTable.IO as Table
import qualified Data.Vector.Mutable as IOVector
import qualified TPM.GraphDB.DIOVector as DIOVector
import qualified TPM.GraphDB.GetT as GetT; import TPM.GraphDB.GetT (GetT)

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
-- run :: Cereal.Get (IO (Node db a))
-- run root = do
--   state <- Table.new
--   undefined

type Deserialize db = ReaderT (State db) (GetT IO)
type State db = DIOVector.DIOVector (AnyNode db)

safeGet :: (SafeCopy a) => Deserialize db a
safeGet = lift $ GetT.liftGet $ SafeCopy.safeGet

getNode :: forall db. (Typeable db, SafeCopy (Term db)) => Deserialize db (AnyNode db)
getNode = do
  node <- do
    term :: Term db <- safeGet
    node <- liftIO $ Node.new (fromTerm term)
    return $ AnyNode node
  do
    state <- ask
    liftIO $ DIOVector.append state node
  return node

-- getNode = do
--   term :: Int <- tell $ SafeCopy.safeGet
--   b :: Char <- ask
--   lift $ lift $ putStrLn "dsf"



-- populateNode :: AnyNode db -> Deserialize db ()
populateNode (AnyNode node) = undefined



