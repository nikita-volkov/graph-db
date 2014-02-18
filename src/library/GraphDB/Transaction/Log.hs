{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Transaction.Log where

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


-- newtype Build u m r = 
--   Build (WriterT [Entry u] (StateT Int m) r) 
--   deriving (Functor, Applicative, Monad, MonadIO)

-- runBuild :: Build u m r -> m r
-- runBuild (Build t) = runStateT (runWriterT [] t) 0

-- execBuild :: Build u m r -> m (Log u)
-- execBuild = runBuild >>> \(_, list) -> Log list

-- instance MonadIO m => Tx (Build u m) where
--   type Ref (Build u m) = Int
--   getRoot = Build $ do
--     tell GetRoot
--     modify succ >> get
--   getTargetsByType ref t = Build $ do
--     tell $ GetTargetsByType ref t
--     -- querying without returning the result is dumb!
--     mapM newRef =<< do liftIO $ Node.getTargetsByType node t


