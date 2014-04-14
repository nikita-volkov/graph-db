
import Benchmarks.Prelude
import Criterion.Main
import qualified GraphDB as G
import qualified Benchmarks.GraphDB as BG
import qualified Benchmarks.Random as R
import qualified Benchmarks.Postgres as P
import qualified Benchmarks.Postgres.PostgreSQLSimplePlus as P
import qualified Benchmarks.Model as Mod


main = do
  gen <- R.newGen
  defaultMain
    [
      bgroup "Inserting" $ let 
        populationSettings = (20, 100, 1000-100-20)
        in 
          [
            bench "Postgres" $
              postgresSessionBenchmark gen 1 $ postgresPopulationSession populationSettings,
            bgroup "GraphDB" $ let
              session = graphDBPopulationSession populationSettings
              in
                [
                  bench "Client of a Persistent Host" $ 
                    graphDBClientSessionBenchmark gen True False session,
                  bench "Persistent" $ 
                    graphDBPersistentSessionBenchmark gen session,
                  bench "Nonpersistent" $ 
                    graphDBNonpersistentSessionBenchmark gen session
                ]
          ]
    ]


-- * Shared Settings
-------------------------

type PopulationSettings = (GenresNum, ArtistsNum, SongsNum)
type GenresNum = Int
type ArtistsNum = Int
type SongsNum = Int


-- * GraphDB Subject
-------------------------

type GraphDBSession = 
  forall s u m.
  (G.Session s, u ~ BG.Catalogue,
   MonadIO (s u (R.GenT m)), MonadTrans (s u),
   MonadIO m, MonadBaseControl IO m) => 
  s u (R.GenT m) ()

graphDBPopulationSession :: PopulationSettings -> GraphDBSession
graphDBPopulationSession (genresNum, artistsNum, songsNum) = do
  -- Insert genres:
  replicateM_ genresNum $ do
    name <- lift $ R.generateName
    G.write $ BG.insertGenre $ BG.Genre name
  -- Insert artists:
  replicateM_ artistsNum $ do
    name <- lift $ R.generateName
    G.write $ BG.insertArtist $ BG.Artist name
  -- Insert songs:
  replicateM_ songsNum $ do
    name <- lift $ R.generateName
    genres <- do
      length <- lift $ R.generateVariate (1, 5)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, genresNum)
        return $ BG.UID n
    artists <- do
      length <- lift $ R.generateVariate (1, 2)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, artistsNum)
        return $ BG.UID n
    G.write $ BG.insertSong (BG.Song name) genres artists

graphDBNonpersistentSessionBenchmark :: R.Gen -> GraphDBSession -> IO ()
graphDBNonpersistentSessionBenchmark gen a = do
  R.runGenT gen $ BG.runNonpersistentSession $ a

graphDBPersistentSessionBenchmark :: R.Gen -> GraphDBSession -> IO ()
graphDBPersistentSessionBenchmark gen a = do
  BG.initDir
  R.runGenT gen $ void $ BG.runPersistentSession $ a

graphDBClientSessionBenchmark :: R.Gen -> Bool -> Bool -> GraphDBSession -> IO ()
graphDBClientSessionBenchmark gen persistent socket a = do
  BG.initDir
  if persistent
    then 
      void $ BG.runPersistentSession $
      BG.serve socket $ lift $ R.runGenT gen $ BG.runClientSession socket $ a
    else 
      void $ BG.runNonpersistentSession $
      BG.serve socket $ lift $ R.runGenT gen $ BG.runClientSession socket $ a


-- * Postgres Subject
-------------------------

type PostgresSession = P.Session (R.GenT IO)

postgresPopulationSession :: PopulationSettings -> PostgresSession ()
postgresPopulationSession (genresNum, artistsNum, songsNum) = do
  replicateM_ genresNum $ do
    name <- lift $ R.generateName
    P.runAction False $ P.insertGenre $ P.Genre name
  replicateM_ artistsNum $ do
    name <- lift $ R.generateName
    P.runAction False $ P.insertArtist $ P.Artist name
  replicateM_ songsNum $ do
    name <- lift $ R.generateName
    genres <- do
      length <- lift $ R.generateVariate (1, 5)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, genresNum)
        return $ P.UID n
    artists <- do
      length <- lift $ R.generateVariate (1, 2)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, artistsNum)
        return $ P.UID n
    P.runAction True $ P.insertSong (P.Song name) genres artists

postgresSessionBenchmark :: R.Gen -> P.PoolSize -> PostgresSession () -> IO ()
postgresSessionBenchmark gen poolSize sess = do
  R.runGenT gen $ P.runSession (host, port, user, pw, db, poolSize) $ do
    P.runAction False P.init
    sess
  where
    host = "localhost"
    port = 5432
    user = "postgres"
    pw = ""
    db = "test"



