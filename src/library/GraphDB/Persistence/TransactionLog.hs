{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence.TransactionLog where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector
import qualified GraphDB.Union as Union
import qualified GraphDB.Graph as Graph
import qualified GraphDB.Transaction.Backend as B


-- |
-- A serializable reproduction of all the modifications to the graph done during a transaction.
newtype Log u = Log [Entry u] deriving (Generic)
instance (Union.Serializable m u) => Serializable m (Log u)

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry u =
  GetRoot |
  NewNode (Union.Value u) |
  GetTargetsByType Ref (Union.Type u) |
  GetTargetsByIndex Ref (Union.Index u) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  GetValue Ref |
  SetValue Ref (Union.Value u)
  deriving (Generic)
instance (Union.Serializable m u) => Serializable m (Entry u)

type Ref = Int

apply :: forall u. (Union.Union u) => Graph.Graph u -> Log u -> IO ()
apply graph (Log actions) = do
  refs <- DIOVector.new
  let
    applyEntry = \case
      GetRoot -> do
        root <- B.getRoot :: B.Tx (Graph.Graph u) (B.Node (Graph.Graph u))
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
      GetValue r -> 
        void $ join $ B.getValue <$> resolveRef r
      SetValue r v -> 
        void $ join $ B.setValue <$> resolveRef r <*> pure v
      where
        appendRef = liftIO . void . DIOVector.append refs
        resolveRef = liftIO . DIOVector.unsafeLookup refs
  flip B.runWrite graph $ forM_ actions applyEntry
  

