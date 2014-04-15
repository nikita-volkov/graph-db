
import Benchmarks.Prelude
import Criterion.Main
import qualified Benchmarks.GraphDB as Gra
import qualified Benchmarks.Random as Ran
import qualified Benchmarks.Postgres as Pos
import qualified Benchmarks.Model as Mod
import qualified Benchmarks.AcidState as Aci


main = do
  gen <- Ran.newGen
  defaultMain
    [
      bgroup "Inserting" $ let 
        session = populationSession 10000
        in 
          [
            bgroup "AcidState" $ 
              [
                benchIO "Local" $ do
                  Aci.initDir
                  Ran.runGenT gen $ Aci.runSession Aci.Local $ session
              ]
            ,
            benchIO "Postgres" $ do
              Ran.runGenT gen $ Pos.runSession 1 $ do
                Pos.init
                Pos.interpretSession session
            ,
            bgroup "GraphDB" $ 
              [
                benchIO "Persistent" $ do
                  Gra.initDir
                  Ran.runGenT gen $ Gra.runPersistentSession $ Gra.interpretSession session
                ,
                benchIO "Nonpersistent" $ do
                  Ran.runGenT gen $ Gra.runNonpersistentSession $ Gra.interpretSession session
              ]
          ]
    ]

benchIO :: String -> IO a -> Benchmark
benchIO = bench


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
