
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
    standoff "Inserting" $ do
      session <- Ran.runGenT gen $ do
        list <- replicateM 1000 $ Ran.generateName
        return $ forM_ list $ Mod.insertArtist . Mod.Artist
      subject "Postgres" $ do
        pause
        Pos.runSession 1 $ do
          Pos.init
          lift $ continue
          Pos.interpretSession session
          lift $ pause
      group "AcidState" $ do
        subject "Nonpersistent" $ do
          pause
          liftIO $ Aci.initDir
          r <- Aci.runSession Aci.LocalNonpersistent $ do
            lift $ continue
            !r <- session
            lift $ pause
          nfIO $ return r
        subject "Persistent" $ do
          pause
          liftIO $ Aci.initDir
          Aci.runSession Aci.LocalPersistent $ do
            lift $ continue
            session
            lift $ pause
      group "GraphDB" $ do
        subject "Persistent" $ do
          pause
          liftIO $ Gra.initDir
          Gra.runPersistentSession $ do
            lift $ continue
            Gra.interpretSession session
            lift $ pause
        subject "Nonpersistent" $ do
          pause
          Gra.runNonpersistentSession $ do
            lift $ continue
            Gra.interpretSession session
            lift $ pause

