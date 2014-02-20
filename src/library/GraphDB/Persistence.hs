{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence where

import GraphDB.Util.Prelude
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Union as U
import qualified GraphDB.Storage as S
import qualified GraphDB.Persistence.TransactionLog as TL
import qualified GraphDB.Graph as G

data Persistence u = Persistence !(G.Graph u) !(Storage u)
type Storage u = S.Storage (Maybe (U.Node u)) (TL.Log u)
type Tx u = RWST () [TL.Entry u] Int (B.Tx (G.Graph u))

instance (U.Union u) => B.Backend (Persistence u) where
  type Tx (Persistence u) = Tx u
  data Node (Persistence u) = Node Int (B.Node (G.Graph u))
  type Value (Persistence u) = U.Value u
  type Type (Persistence u) = U.Type u
  type Index (Persistence u) = U.Index u
  runRead tx (Persistence g s) = do
    (r, _) <- B.runRead (evalRWST tx () 0) g
    return r
  runWrite tx (Persistence g s) = do
    (r, entries) <- B.runWrite (evalRWST tx () 0) g
    S.persistEvent s $ TL.Log entries
    return r
  getRoot = do
    tell $ pure $ TL.GetRoot
    newGraphNodeTx =<< do lift $ B.getRoot
  addTarget (Node si sn) (Node ti tn) = do
    tell $ pure $ TL.AddTarget si ti
    lift $ B.addTarget sn tn

newGraphNodeTx :: B.Node (G.Graph u) -> Tx u (B.Node (Persistence u))
newGraphNodeTx n = do
  index <- get
  modify succ
  return $ Node index n

