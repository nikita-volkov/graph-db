{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistent.Log where

import GraphDB.Util.Prelude

import qualified GraphDB.Action as A
import qualified GraphDB.Graph as G
import qualified GraphDB.Util.FileSystem
import qualified GraphDB.Util.DIOVector as V


-- * Log
-------------------------

type Log s = [Entry s]

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry s =
  GetRoot |
  NewNode (G.Value s) |
  GetTargetsByIndex Node (G.Index s) |
  AddTarget Node Node |
  RemoveTarget Node Node |
  Remove Node |
  SetValue Node (G.Value s)
  deriving (Generic)

instance (Serializable m (G.Value s), Serializable m (G.Index s)) => Serializable m (Entry s)

type Node = Int


-- * Action
-------------------------

toAction :: MonadIO m => Log s -> A.Action n (G.Value s) (G.Index s) m ()
toAction log = do
  refs <- liftIO $ V.new
  let
    appendRef = liftIO . void . V.append refs
    resolveRef = liftIO . V.unsafeLookup refs
    applyEntry = \case
      GetRoot -> A.getRoot >>= appendRef
      NewNode v -> A.newNode v >>= appendRef
      GetTargetsByIndex r i -> resolveRef r >>= flip A.getTargetsByIndex i >>= mapM_ appendRef
      AddTarget s t -> void $ join $ A.addTarget <$> resolveRef s <*> resolveRef t
      RemoveTarget s t -> void $ join $ A.removeTarget <$> resolveRef s <*> resolveRef t
      Remove r -> A.remove =<< resolveRef r
      SetValue r v -> void $ join $ A.setValue <$> resolveRef r <*> pure v
  mapM_ applyEntry log
