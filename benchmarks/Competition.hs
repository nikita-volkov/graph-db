
import Benchmarks.Prelude
import Criterion.Main
import qualified GraphDB as G
import qualified Benchmarks.GraphDB as BG
import qualified Benchmarks.Random as R
import qualified Benchmarks.Postgres as P
import qualified Benchmarks.Model as Mod


main = do
  gen <- R.newGen
  defaultMain
    [
      bgroup "Inserting" $ let 
        session = populationSession (20, 100, 1000-100-20)
        in 
          [
            benchIO "Postgres" $ do
              R.runGenT gen $ P.runSession 1 $ do
                P.init
                P.interpretSession session
            ,
            bgroup "GraphDB" $ 
              [
                benchIO "Persistent" $ do
                  BG.initDir
                  R.runGenT gen $ BG.runPersistentSession $ BG.interpretSession session
                ,
                benchIO "Nonpersistent" $ do
                  R.runGenT gen $ BG.runNonpersistentSession $ BG.interpretSession session
              ]
          ]
    ]

benchIO :: String -> IO a -> Benchmark
benchIO = bench


-- * Population
-------------------------

type PopulationSettings = (GenresNum, ArtistsNum, SongsNum)
type GenresNum = Int
type ArtistsNum = Int
type SongsNum = Int

populationSession :: (MonadIO m) => PopulationSettings -> Mod.Session (R.GenT m) ()
populationSession (genresNum, artistsNum, songsNum) = do
  replicateM_ genresNum $ do
    name <- lift $ R.generateName
    Mod.insertGenre $ Mod.Genre name
  replicateM_ artistsNum $ do
    name <- lift $ R.generateName
    Mod.insertArtist $ Mod.Artist name
  replicateM_ songsNum $ do
    name <- lift $ R.generateName
    genres <- do
      length <- lift $ R.generateVariate (1, 5)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, genresNum)
        return $ Mod.UID n
    artists <- do
      length <- lift $ R.generateVariate (1, 2)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, artistsNum)
        return $ Mod.UID n
    Mod.insertSong (Mod.Song name) genres artists

