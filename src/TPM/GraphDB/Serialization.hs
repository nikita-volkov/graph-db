module TPM.GraphDB.Serialization where

import TPM.Prelude
import qualified Data.SafeCopy as SafeCopy
import qualified Data.Serialize as Cereal
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified Data.HashTable.IO as Table

type Table k v = Table.BasicHashTable k v
type SafeCopy = SafeCopy.SafeCopy



class IsTerm db a where
  toTerm :: a -> Term db
  fromTerm :: Term db -> a

-- | 
-- A union type for all nodes, edges and events under a certain tag.
-- Used as a dictionary for deserialization.
-- Client specifies a 'SafeCopy' instance for it.
data family Term db



-- |
-- A single serializer on a root node.
-- Manages a shared registry of all nodes, thus elluding repeated serialization of same data.
-- 
-- Strategy A:
-- 1. Put a dictionary of all nodes by refs.
-- 
-- Strategy B:
-- 1. Put a node's value.
-- 2. Put all its edges with refs to other nodes.
-- 3. Traverse all referred nodes by going to step 1 with each of them.
-- 
-- FIXME: accumulating 'Cereal.Put' probably is very ineffective.
serialize :: (IsTerm db (), SafeCopy (Term db)) => Node db () -> IO Cereal.Put
serialize root = do
  -- nodeByIndexTable <- Table.new :: IO (Table Int (forall a. Node db a))
  -- processedNodes <- newIORef [] :: IO (IORef [forall a. Node db a])
  indexByNode <- newIORef [] :: IO (IORef [forall a. (Node db a, Word64)])

  let processNode :: forall db a. (SafeCopy (Term db), IsTerm db a) => 
                     Node db a -> IO (Cereal.Put)
      processNode node = do
        putValue <- do
          value <- Node.getValue node
          return $ SafeCopy.safePut (toTerm value :: Term db)
        putEdges <- do
          let edges = Node.edges node
          undefined
        undefined

  processNode root 

deserialize :: Cereal.Get (IO (Node db ()))
deserialize = do
  undefined
  where
    populateNode :: Node db a -> 
                    ReaderT (Table Int (forall a. Node db a)) Cereal.Get (IO ())
    populateNode node = undefined
