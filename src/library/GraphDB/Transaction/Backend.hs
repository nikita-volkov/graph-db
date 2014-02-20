module GraphDB.Transaction.Backend where

import GraphDB.Util.Prelude


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
  type Value b
  type Type b
  type Index b
  runWrite :: Tx b r -> b -> IO r
  runRead :: Tx b r -> b -> IO r
  getRoot :: Tx b (Node b)
  getValue :: Node b -> Tx b (Value b)
  setValue :: Node b -> Value b -> Tx b ()
  getTargetsByType :: Node b -> Type b -> Tx b [Node b]
  getTargetsByIndex :: Node b -> Index b -> Tx b [Node b]
  addTarget :: Node b -> Node b -> Tx b Bool
  removeTarget :: Node b -> Node b -> Tx b Bool
  getStats :: Node b -> Tx b (Int, Int)


