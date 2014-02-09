module GraphDB.Transaction.Node where

import GraphDB.Util.Prelude


-- | A graph API required by transactions.
class Node n where
  type Value n
  type Index n
  type Type n
  getTargetsByType :: n -> Type n -> IO [n]
  new :: Value n -> IO n
  addTarget :: n -> n -> IO Bool

