module GraphDB.Graph where

import GraphDB.Util.Prelude
import qualified GraphDB.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Graph.Node as N
import qualified Control.Concurrent.FairRWLock as L


type Session u m = ReaderT (U.Node u) (ReaderT L.RWLock m)

type Action u = A.Action (U.Node u) (U.Value u) (U.Type u) (U.Index u)

type Node u = U.Node u

runSession :: (MonadIO m) => U.Node u -> Session u m r -> m r
runSession n s = do
  l <- liftIO $ L.new
  flip runReaderT l $ flip runReaderT n $ s

runTransaction :: (MonadBaseControl IO m) => Bool -> Session u m r -> Session u m r
runTransaction write tx = do
  l <- lift $ ask
  if write
    then control $ \runInBase -> L.withWrite l $ runInBase tx
    else control $ \runInBase -> L.withRead l $ runInBase tx

runAction :: (MonadBase IO m, U.Union u) => Action u m r -> Session u m r
runAction = ((lift . lift) .) $ iterT $ \case
  A.NewNode v c -> N.new v |> liftBase >>= c
