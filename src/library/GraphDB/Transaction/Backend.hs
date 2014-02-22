module GraphDB.Transaction.Backend where

import GraphDB.Util.Prelude


-- | A low level interface for types capable of executing transactions.
class (MonadIO (Tx b), Applicative (Tx b)) => Backend b where
  -- | 
  -- A low level transaction which both Read and Write revolve around.
  type Tx b
  -- |
  -- Is a @data@ for instance disambiguation.
  -- Really the 'Tx' should be one instead, but due to
  -- https://ghc.haskell.org/trac/ghc/ticket/4185
  -- it comes with problems.
  data Node b
  data Value b
  data Type b
  data Index b
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

class (Backend b) => PolyValue b v where
  packValue :: v -> (Type b, Value b)
  unpackValue :: Value b -> Maybe v

class (Backend b) => PolyIndex b i where
  packIndex :: i -> Index b

