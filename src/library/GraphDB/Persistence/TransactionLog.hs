{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Persistence.TransactionLog where

import GraphDB.Util.Prelude
import qualified GraphDB.Util.DIOVector as DIOVector
import qualified GraphDB.Union as Union
import qualified GraphDB.Graph as Graph
import qualified GraphDB.Transaction.Backend as Backend


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
  SetValue (Union.Value u) Ref
  deriving (Generic)
instance (Union.Serializable m u) => Serializable m (Entry u)

type Ref = Int

apply :: forall u. (Union.Union u) => Graph.Graph u -> Log u -> IO ()
apply graph (Log actions) = do
  refs <- DIOVector.new
  let
    applyEntry = \case
      GetRoot -> do
        root <- Backend.getRoot :: Backend.Tx (Graph.Graph u) (Backend.Node (Graph.Graph u))
        liftIO $ void $ DIOVector.append refs root
      NewNode value -> $notImplemented
      GetTargetsByType ref typ -> do
        mapM_ (liftIO . DIOVector.append refs) =<< do
          node <- liftIO $ DIOVector.unsafeLookup refs ref
          Backend.getTargetsByType node typ
      GetTargetsByIndex index ref -> $notImplemented
      AddTarget sRef tRef -> do
        source <- liftIO $ DIOVector.unsafeLookup refs sRef
        target <- liftIO $ DIOVector.unsafeLookup refs tRef
        Backend.addTarget source target
        return ()
      _ -> $notImplemented
  flip Backend.runWrite graph $ forM_ actions applyEntry
  

