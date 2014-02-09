module GraphDB.Persistence.Log.Transaction where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector; import GraphDB.Util.DIOVector (DIOVector)
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)

-- |
-- A serializable representation of a granular transaction action.
-- Essential for persistence.
data Action t =
  GetRoot |
  NewNode (Node.Value t) |
  GetTargetsByType Ref t |
  GetTargetsByIndex Ref (Node.Index t) |
  AddTarget Ref Ref |
  RemoveTarget Ref Ref |
  GetValue Ref |
  SetValue (Node.Value t) Ref
  
type Ref = Int

newtype Transaction t = Transaction [Action t]

applyTransaction :: (Node.Type t) => Node t -> Transaction t -> IO ()
applyTransaction root (Transaction actions) = do
  refs <- DIOVector.new
  let
    applyAction = \case
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
  forM_ actions applyAction
