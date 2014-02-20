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
type Tx u = RWST (G.Graph u) [TL.Entry u] Int IO

instance (U.Union u) => B.Backend (Persistence u) where
  type Tx (Persistence u) = Tx u
  newtype Node (Persistence u) = Node (Int, B.Node (G.Graph u))
  type Value (Persistence u) = U.Value u
  type Type (Persistence u) = U.Type u
  type Index (Persistence u) = U.Index u
  runTx tx (Persistence g s) = do
    (r, entries) <- evalRWST tx g 0
    S.persistEvent s $ TL.Log entries
    return r
  getRoot = do
    tell $ pure $ TL.GetRoot
    newNodeTx =<< do liftGraphTx $ B.getRoot

newNodeTx :: B.Node (G.Graph u) -> Tx u (B.Node (Persistence u))
newNodeTx n = do
  index <- get
  modify succ
  return $ Node (index, n)

liftGraphTx :: (U.Union u) => B.Tx (G.Graph u) r -> Tx u r
liftGraphTx gtx = do
  g <- ask
  liftIO $ B.runTx gtx g

