module GraphDB.Transaction.Backend where

import GraphDB.Util.Prelude
import qualified GraphDB.Model.Union as U

-- | A low level interface for types capable of executing transactions.
class (MonadIO (Tx b u), Applicative (Tx b u)) => Backend b u where
  -- | 
  -- A low level transaction which both Read and Write revolve around.
  data Tx b u r
  type Node b u
  runWrite :: Tx b u r -> b -> IO r
  runRead :: Tx b u r -> b -> IO r
  newNode :: U.Value u -> Tx b u (Node b u)
  getValue :: Node b u -> Tx b u (U.Value u)
  setValue :: Node b u -> U.Value u -> Tx b u ()
  getRoot :: Tx b u (Node b u)
  getTargetsByType :: Node b u -> U.Type u -> Tx b u [Node b u]
  getTargetsByIndex :: Node b u -> U.Index u -> Tx b u [Node b u]
  addTarget :: Node b u -> Node b u -> Tx b u Bool
  removeTarget :: Node b u -> Node b u -> Tx b u Bool
  getStats :: Node b u -> Tx b u (Int, Int)
