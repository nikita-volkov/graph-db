{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence.TransactionLog where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector
import qualified GraphDB.Union as Union
import qualified GraphDB.Engine.Node as Node

-- |
-- A serializable reproduction of all the modifications to the graph done during a transaction.
newtype Log u = Log [Entry u] deriving (Generic)
instance (Serializable m (Union.Index u), Serializable m (Union.Type u), Serializable m (Union.Value u)) => Serializable m (Log u)

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
  SetValue (Union.Value u) Ref
  deriving (Generic)
instance (Serializable m (Union.Index u), Serializable m (Union.Type u), Serializable m (Union.Value u)) => Serializable m (Entry u)

type Ref = Int

apply :: forall u. (Union.Union u) => Union.Node u -> Log u -> IO ()
apply root (Log actions) = do
  refs <- DIOVector.new
  let
    applyEntry = \case
      GetRoot -> do
        void $ DIOVector.append refs root
      NewNode value -> $notImplemented
      GetTargetsByType ref typ -> do
        mapM_ (DIOVector.append refs) =<< do
          node <- DIOVector.unsafeLookup refs ref
          Node.getTargetsByType node typ
      GetTargetsByIndex index ref -> $notImplemented
      AddTarget sRef tRef -> do
        source <- DIOVector.unsafeLookup refs sRef
        target <- DIOVector.unsafeLookup refs tRef
        Node.addTarget source target
        return ()
      _ -> $notImplemented
  forM_ actions applyEntry

