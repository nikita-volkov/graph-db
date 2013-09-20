module TPM.GraphDB.Node where

import TPM.Prelude
import qualified Data.Map as Map



insertEdge :: Edge s t -> Node t -> Node s -> IO ()
insertEdge = undefined

new :: v -> IO (Node v)
new value = Node <$> newIORef value <*> newIORef Map.empty

getValue :: Node v -> IO v
getValue (Node ref _) = readIORef ref



data Node v = Node 
  (IORef v)
  (IORef (Map TypeRep (forall t. Typeable t => Map (Edge v t) [Node t])))

data family Edge source target



-- newtype Update result = Update (IO result)

-- newtype IO result = IO (IO result)



-- -- runUpdate :: Update r -> IO r
-- -- runUpdate = undefined

-- insertEdge :: Edge s t -> Node s -> Node t -> Update ()
-- insertEdge edge source target = undefined

-- insertNode :: v -> Update (Node v)
-- insertNode value = Update $ Node <$> newIORef value <*> newIORef Map.empty

-- -- | Set a value for some node
-- set :: v -> Node v -> Update ()
-- set = undefined


-- getTargets :: Edge s t -> Node s -> IO [Node t]
-- getTargets edge (Node _ edgesRef) = IO $ do
--   edges <- readIORef edgesRef
--   undefined




