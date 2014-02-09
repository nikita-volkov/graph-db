{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Transaction.Log where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector; import GraphDB.Util.DIOVector (DIOVector)
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)

-- |
-- A serializable reproduction of all the modifications to the graph done during a transaction.
newtype Log t = Log [Entry t] deriving (Generic)
instance (Serializable m (Node.Index t), Serializable m t, Serializable m (Node.Value t)) => Serializable m (Log t)

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Entry t =
  GetRoot |
  NewNode (Node.Value t) |
  GetTargetsByType Ref t |
  GetTargetsByIndex Ref (Node.Index t) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  GetValue Ref |
  SetValue (Node.Value t) Ref
  deriving (Generic)
instance (Serializable m (Node.Index t), Serializable m t, Serializable m (Node.Value t)) => Serializable m (Entry t)

type Ref = Int

apply :: (Node.Type t) => Node t -> Log t -> IO ()
apply root (Log actions) = do
  refs <- DIOVector.new
  let
    applyEntry = \case
      GetRoot -> do
        DIOVector.append refs root
        return ()
      NewNode value -> $notImplemented
      GetTargetsByType ref typ -> do
        node <- DIOVector.unsafeLookup refs ref
        targets <- Node.getTargetsByType node typ
        forM_ targets $ DIOVector.append refs
      GetTargetsByIndex index ref -> $notImplemented
      AddTarget sRef tRef -> do
        source <- DIOVector.unsafeLookup refs sRef
        target <- DIOVector.unsafeLookup refs tRef
        Node.addTarget source target
        return ()
      _ -> $notImplemented
  forM_ actions applyEntry
