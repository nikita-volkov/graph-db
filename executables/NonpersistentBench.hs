
import Benchmarks.Prelude
import qualified CriterionPlus as Cri
import qualified Benchmarks.GraphDB as Gra
import qualified Benchmarks.Random as Ran
import qualified Benchmarks.Model as Mod


main = do
  gen <- Ran.newGen
  Cri.benchmark $ do
    Cri.standoff "Inserting" $ do
      let subject n = do
            session <- Ran.runGenT gen $ do
              list <- replicateM n $ Ran.generateName
              return $ forM_ list $ Mod.insertArtist . Mod.Artist
            Cri.subject (cs $ show n) $ do
              Cri.pause
              Gra.runNonpersistentSession $ do
                lift $ Cri.continue
                Gra.interpretSession session
                lift $ Cri.pause
      subject 21000
      subject 16000
      subject 11000
      subject 6000
      subject 1000
