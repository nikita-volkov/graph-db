{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Transaction.Log where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector
import qualified GraphDB.Transaction.Node as Node

-- |
-- A serializable reproduction of all the modifications to the graph done during a transaction.
newtype Log n = Log [Entry n] deriving (Generic)
instance (Serializable m (Node.Index n), Serializable m (Node.Type n), Serializable m (Node.Value n)) => Serializable m (Log n)

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry n =
  GetRoot |
  NewNode (Node.Value n) |
  GetTargetsByType Ref (Node.Type n) |
  GetTargetsByIndex Ref (Node.Index n) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  GetValue Ref |
  SetValue (Node.Value n) Ref
  deriving (Generic)
instance (Serializable m (Node.Index n), Serializable m (Node.Type n), Serializable m (Node.Value n)) => Serializable m (Entry n)

type Ref = Int

apply :: forall n. (Node.Node n) => n -> Log n -> IO ()
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
