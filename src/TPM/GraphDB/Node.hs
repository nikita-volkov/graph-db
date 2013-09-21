module TPM.GraphDB.Node where

import TPM.Prelude
import qualified Data.HashTable.IO as HashTables



insertEdge :: Edge s t -> Node t -> Node s -> IO ()
insertEdge = undefined

deleteEdge :: Edge s t -> Node t -> Node s -> IO ()
deleteEdge = undefined

new :: v -> IO (Node v)
new value = Node <$> newIORef value <*> HashTables.new

getValue :: Node v -> IO v
getValue (Node ref _) = readIORef ref

setValue :: v -> Node v -> IO ()
setValue = undefined

getTargets :: Edge s t -> Node s -> IO [Node t]
getTargets edge (Node _ edgesTable) = do
  undefined



data Node v = Node 
  (IORef v)
  (HashTables.BasicHashTable TypeRep (forall t. Typeable t => HashTables.BasicHashTable (Edge v t) [Node t]))

-- |
-- An edge from /source/ value to /target/.
data family Edge source target




