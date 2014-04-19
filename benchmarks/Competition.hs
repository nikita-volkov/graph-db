
import Benchmarks.Prelude hiding (group)
import CriterionPlus
import qualified Benchmarks.GraphDB as Gra
import qualified Benchmarks.Random as Ran
import qualified Benchmarks.Postgres as Pos
import qualified Benchmarks.Model as Mod
import qualified Benchmarks.AcidState as Aci


main = do
  gen <- Ran.newGen
  benchmark $ do
    let session = populationSession 100
    standoff "Inserting" $ do
      subject "Postgres" $ do
        pause
        Ran.runGenT gen $ Pos.runSession 1 $ do
          Pos.init
          lift $ lift $ continue
          Pos.interpretSession session
          lift $ lift $ pause
      group "AcidState" $ do
        subject "Persistent" $ do
          pause
          liftIO $ Aci.initDir
          Ran.runGenT gen $ Aci.runSession Aci.LocalPersistent $ do
            lift $ lift $ continue
            session
            lift $ lift $ pause
        subject "Nonpersistent" $ do
          pause
          liftIO $ Aci.initDir
          Ran.runGenT gen $ Aci.runSession Aci.LocalNonpersistent $ do
            lift $ lift $ continue
            session
            lift $ lift $ pause
      group "GraphDB" $ do
        subject "Persistent" $ do
          pause
          liftIO $ Gra.initDir
          Ran.runGenT gen $ Gra.runPersistentSession $ do
            lift $ lift $ continue
            Gra.interpretSession session
            lift $ lift $ pause
        subject "Nonpersistent" $ do
          pause
          Ran.runGenT gen $ Gra.runNonpersistentSession $ do
            lift $ lift $ continue
            Gra.interpretSession session
            lift $ lift $ pause


-- * Population
-------------------------

-- |
-- We insert only artists, 
-- so that results don't get skewed by lookups in some databases.
populationSession :: (MonadIO m) => Int -> Mod.Session (Ran.GenT m) ()
populationSession num = do
  replicateM_ num $ do
    name <- lift $ Ran.generateName
    Mod.insertArtist $ Mod.Artist name
