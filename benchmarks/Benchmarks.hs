
import Benchmarks.Prelude
import Criterion.Main
import qualified GraphDB as G
import qualified Benchmarks.GraphDB as BG
import qualified Benchmarks.Random as R


main = defaultMain
  [
    bgroup "Populating" $ 
      [
        bench "GraphDB, Nonpersistent" $ 
          GraphDBNonpersistentSessionBenchmark graphDBPopulatingSession,
        bench "GraphDB, Persistent" $ 
          GraphDBPersistentSessionBenchmark graphDBPopulatingSession,
        bench "GraphDB, Client, Nonpersistent, Socket" $ 
          GraphDBClientSessionBenchmark False True graphDBPopulatingSession,
        bench "GraphDB, Client, Nonpersistent, Host" $ 
          GraphDBClientSessionBenchmark False False graphDBPopulatingSession,
        bench "GraphDB, Client, Persistent, Socket" $ 
          GraphDBClientSessionBenchmark True True graphDBPopulatingSession,
        bench "GraphDB, Client, Persistent, Host" $ 
          GraphDBClientSessionBenchmark True False graphDBPopulatingSession
      ]
  ]


-- GraphDB
-------------------------

type GraphDBSession = 
  forall s u m.
  (G.Session s, u ~ BG.Catalogue,
   Monad (s u (R.GenT m)), MonadTrans (s u),
   MonadIO m, MonadBaseControl IO m, 
   MonadIO (s BG.Catalogue (R.GenT m))) => 
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
    artistsNum = 400
    songsNum = 3000


newtype GraphDBNonpersistentSessionBenchmark = GraphDBNonpersistentSessionBenchmark GraphDBSession

instance Benchmarkable GraphDBNonpersistentSessionBenchmark where
  run (GraphDBNonpersistentSessionBenchmark a) n = do
    R.runGenT $ do
      replicateM_ n $ do
        BG.runNonpersistentSession $ a

newtype GraphDBPersistentSessionBenchmark = GraphDBPersistentSessionBenchmark GraphDBSession

instance Benchmarkable GraphDBPersistentSessionBenchmark where
  run (GraphDBPersistentSessionBenchmark a) n = do
    R.runGenT $ do
      replicateM_ n $ do
        liftIO $ BG.initDir
        BG.runPersistentSession $ a

data GraphDBClientSessionBenchmark = GraphDBClientSessionBenchmark Bool Bool GraphDBSession

instance Benchmarkable GraphDBClientSessionBenchmark where
  run (GraphDBClientSessionBenchmark persistent socket a) n = do
    R.runGenT $ do
      gen <- ask
      replicateM_ n $ do
        liftIO $ BG.initDir
        if persistent
          then void $ BG.runPersistentSession $
            BG.serve socket $ lift $ flip runReaderT gen $ BG.runClientSession socket $ a
          else void $ BG.runNonpersistentSession $
            BG.serve socket $ lift $ flip runReaderT gen $ BG.runClientSession socket $ a



