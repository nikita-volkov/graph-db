{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistent.Log where

import GraphDB.Util.Prelude

import qualified GraphDB.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Util.FileSystem
import qualified GraphDB.Util.DIOVector as DV


-- * Log
-------------------------

type Log u = [Entry u]

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry u =
  GetRoot |
  NewNode (U.Value u) |
  GetTargetsByType NodeRef (U.Type u) |
  GetTargetsByIndex NodeRef (U.Index u) |
  AddTarget NodeRef NodeRef |
  RemoveTarget NodeRef NodeRef |
  SetValue NodeRef (U.Value u)
  deriving (Generic)

instance (U.Serializable m u) => Serializable m (Entry u)

type NodeRef = Int


-- * Action
-------------------------

toAction :: MonadIO m => Log u -> A.Action n (U.Value u) (U.Type u) (U.Index u) m ()
toAction log = do
  refs <- liftIO $ DV.new
  let
    appendRef = liftIO . void . DV.append refs
    resolveRef = liftIO . DV.unsafeLookup refs
    applyEntry = \case
      GetRoot -> A.getRoot >>= appendRef
      NewNode v -> A.newNode v >>= appendRef
      GetTargetsByType r t -> resolveRef r >>= flip A.getTargetsByType t >>= mapM_ appendRef
      GetTargetsByIndex r i -> resolveRef r >>= flip A.getTargetsByIndex i >>= mapM_ appendRef
      AddTarget s t -> void $ join $ A.addTarget <$> resolveRef s <*> resolveRef t
      RemoveTarget s t -> void $ join $ A.removeTarget <$> resolveRef s <*> resolveRef t
      SetValue r v -> void $ join $ A.setValue <$> resolveRef r <*> pure v
  mapM_ applyEntry log
