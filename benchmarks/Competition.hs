
import Benchmarks.Prelude
import Criterion.Main
import qualified GraphDB as G
import qualified Benchmarks.GraphDB as BG
import qualified Benchmarks.Random as R


main = do
  gen <- R.newGen
  defaultMain
    [
      bgroup "Populating" $ 
        [
          bench "GraphDB, Client, Persistent, Host" $ 
            graphDBClientSessionBenchmark gen True False graphDBPopulatingSession,
          bench "GraphDB, Client, Persistent, Socket" $ 
            graphDBClientSessionBenchmark gen True True graphDBPopulatingSession,
          bench "GraphDB, Client, Nonpersistent, Host" $ 
            graphDBClientSessionBenchmark gen False False graphDBPopulatingSession,
          bench "GraphDB, Client, Nonpersistent, Socket" $ 
            graphDBClientSessionBenchmark gen False True graphDBPopulatingSession,
          bench "GraphDB, Persistent" $ 
            graphDBPersistentSessionBenchmark gen graphDBPopulatingSession,
          bench "GraphDB, Nonpersistent" $ 
            graphDBNonpersistentSessionBenchmark gen graphDBPopulatingSession
        ]
    ]


-- GraphDB
-------------------------

type GraphDBSession = 
  forall s u m.
  (G.Session s, u ~ BG.Catalogue,
   MonadIO (s u (R.GenT m)), MonadTrans (s u),
   MonadIO m, MonadBaseControl IO m) => 
  s u (R.GenT m) ()

graphDBPopulatingSession :: GraphDBSession
graphDBPopulatingSession = do
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
      length <- lift $ R.generateVariate (1, 5)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, artistsNum)
        return $ BG.UID n
    G.write $ BG.insertSong (BG.Song name) genres artists
  where
    genresNum = 20
    artistsNum = 100
    songsNum = 1000

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



