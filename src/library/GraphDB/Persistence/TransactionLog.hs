{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence.TransactionLog where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector
import qualified GraphDB.Transaction.Backend as B


-- |
-- A serializable reproduction of all the modifications to the graph done during a transaction.
newtype Log b = Log [Entry b] deriving (Generic)
instance (Serializable m (Entry b)) => Serializable m (Log b)

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry b =
  GetRoot |
  NewNode (B.Value b) |
  GetTargetsByType Ref (B.Type b) |
  GetTargetsByIndex Ref (B.Index b) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  SetValue Ref (B.Value b)
  deriving (Generic)
instance (Serializable m (B.Index b), Serializable m (B.Type b), Serializable m (B.Value b)) => 
         Serializable m (Entry b)

type Ref = Int

apply :: forall b. (B.Backend b) => b -> Log b -> IO ()
apply graph (Log actions) = do
  refs <- DIOVector.new
  let
    applyEntry = \case
      GetRoot -> do
        root <- B.getRoot :: B.Tx b (B.Node b)
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
  

