module GraphDB.Transaction.Tx where

import GraphDB.Util.Prelude


-- | 
-- A low level transaction which both Read and Write revolve around.
newtype Tx b r = Tx (ReaderT b IO r)
  deriving (Monad, Functor, Applicative, MonadIO, MonadReader b)

runTxReader :: Tx b r -> b -> IO r
runTxReader (Tx reader) = runReaderT reader


class Backend b where
  type Node b
  type Value b
  type Type b
  type Index b
  runTx :: Tx b r 
        -> Bool -- ^ Is it a write?
        -> b
        -> IO r
  getRoot :: Tx b (Node b)
  getValue :: Node b -> Tx b (Value b)
  setValue :: Node b -> Value b -> Tx b ()
  getTargetsByType :: Node b -> Type b -> Tx b [Node b]
  getTargetsByIndex :: Node b -> Index b -> Tx b [Node b]
  addTarget :: Node b -> Node b -> Tx b Bool
  removeTarget :: Node b -> Node b -> Tx b Bool
  getStats :: Node b -> Tx b (Int, Int)


