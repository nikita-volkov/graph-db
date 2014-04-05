module GraphDB.Transaction.Backend where

import GraphDB.Util.Prelude
import qualified GraphDB.Model.Union as U

-- | A low level interface for types capable of executing transactions.
class (MonadIO (Tx b), Applicative (Tx b)) => Backend b where
  type Union b
  -- | 
  -- A low level transaction which both Read and Write revolve around.
  data Tx b r
  type Node b
  runWrite :: Tx b r -> b -> IO r
  runRead :: Tx b r -> b -> IO r
  newNode :: Value b -> Tx b (Node b)
  getValue :: Node b -> Tx b (Value b)
  setValue :: Node b -> Value b -> Tx b ()
  getRoot :: Tx b (Node b)
  getTargetsByType :: Node b -> Type b -> Tx b [Node b]
  getTargetsByIndex :: Node b -> Index b -> Tx b [Node b]
  addTarget :: Node b -> Node b -> Tx b Bool
  removeTarget :: Node b -> Node b -> Tx b Bool
  getStats :: Node b -> Tx b (Int, Int)

type Value b = U.Value (Union b)
type Type b = U.Type (Union b)
type Index b = U.Index (Union b)
