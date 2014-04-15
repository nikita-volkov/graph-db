module Benchmarks.AcidState.AcidStatePlus
(
  Session,
  runLocalSession,
  runRemoteSession,
  Aci.Update,
  Aci.Query,
  update,
  query,
  Aci.makeAcidic,
)
where

import Benchmarks.Prelude
import qualified Data.Acid as Aci
import qualified Benchmarks.Util.FileSystem as Fil
import qualified Control.Exception.Lifted as ELi


newtype Session a m r = Session (ReaderT (Aci.AcidState a) m r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)


runLocalSession :: 
  (MonadIO m, MonadBaseControl IO m, Aci.IsAcidic a) => 
  FilePath -> a -> Session a m r -> m r
runLocalSession dir initState (Session m) = ELi.bracket acquire release apply where
  acquire = liftIO $ Aci.openLocalStateFrom (Fil.encodeString dir) initState
  release s = liftIO $ do
    Aci.createCheckpoint s
    Aci.closeAcidState s
  apply s = runReaderT m s

runRemoteSession :: (MonadIO m) => Session a m r -> m r
runRemoteSession = $notImplemented

update :: 
  (MonadIO m, Aci.UpdateEvent e, Aci.EventState e ~ a) => 
  e -> Session a m (Aci.EventResult e)
update e = do
  s <- Session $ ask
  liftIO $ Aci.update s e

query :: 
  (MonadIO m, Aci.QueryEvent e, Aci.EventState e ~ a) => 
  e -> Session a m (Aci.EventResult e)
query e = do
  s <- Session $ ask
  liftIO $ Aci.query s e
