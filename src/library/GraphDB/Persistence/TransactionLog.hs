{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence.TransactionLog where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector
import qualified GraphDB.Transaction.Backend as B
import qualified GraphDB.Model.Union as U


-- |
-- A serializable reproduction of all the modifications to the graph done during a transaction.
newtype Log b u = Log [Entry u] deriving (Generic)
instance (Serializable m (Entry u)) => Serializable m (Log b u)

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry u =
  GetRoot |
  NewNode (U.Value u) |
  GetTargetsByType Ref (U.Type u) |
  GetTargetsByIndex Ref (U.Index u) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  SetValue Ref (U.Value u)
  deriving (Generic)
instance (Serializable m (U.Index u), Serializable m (U.Type u), Serializable m (U.Value u)) => 
         Serializable m (Entry u)

type Ref = Int

apply :: forall b u. (B.Backend b u) => b -> Log b u -> IO ()
apply graph (Log actions) = do
  refs <- DIOVector.new
  let
    applyEntry = \case
      GetRoot -> do
        root <- B.getRoot :: B.Tx b u (B.Node b u)
        appendRef root
      NewNode value -> do
        appendRef =<< B.newNode value
      GetTargetsByType ref typ -> do
        mapM_ appendRef =<< do
          node <- resolveRef ref
          B.getTargetsByType node typ
      GetTargetsByIndex r i ->
        mapM_ appendRef =<< do join $ B.getTargetsByIndex <$> resolveRef r <*> pure i
      AddTarget s t -> 
        void $ join $ B.addTarget <$> resolveRef s <*> resolveRef t
      RemoveTarget s t -> 
        void $ join $ B.removeTarget <$> resolveRef s <*> resolveRef t
      SetValue r v -> 
        void $ join $ B.setValue <$> resolveRef r <*> pure v
      where
        appendRef = liftIO . void . DIOVector.append refs
        resolveRef = liftIO . DIOVector.unsafeLookup refs
  flip B.runWrite graph $ forM_ actions applyEntry
  

