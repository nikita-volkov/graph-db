
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
        bench "GraphDB, Client, Socket" $ 
          GraphDBClientSessionBenchmark True graphDBPopulatingSession,
        bench "GraphDB, Client, Host" $ 
          GraphDBClientSessionBenchmark False graphDBPopulatingSession
      ]
  ]


-- GraphDB
-------------------------

type GraphDBSession = 
  forall s u m.
  (G.Session s, u ~ BG.Catalogue,
   Monad (s u (R.GenT m)), MonadTrans (s u),
   MonadIO m, MonadBaseControl IO m) => 
  s u (R.GenT m) ()

graphDBPopulatingSession :: GraphDBSession
graphDBPopulatingSession = do
  -- Insert genres:
  replicateM_ 20 $ do
    name <- lift $ R.generateName
    G.write $ BG.insertGenre $ BG.Genre name
  -- Insert artists:
  replicateM_ 1000 $ do
    name <- lift $ R.generateName
    G.write $ BG.insertArtist $ BG.Artist name
  -- Insert songs:
  replicateM_ 10000 $ do
    name <- lift $ R.generateName
    genres <- do
      length <- lift $ R.generateVariate (1, 5)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, 20)
        return $ BG.UID n
    artists <- do
      length <- lift $ R.generateVariate (1, 5)
      replicateM length $ do
        n <- lift $ R.generateVariate (1, 1000)
        return $ BG.UID n
    G.write $ BG.insertSong (BG.Song name) genres artists


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

data GraphDBClientSessionBenchmark = GraphDBClientSessionBenchmark Bool GraphDBSession

instance Benchmarkable GraphDBClientSessionBenchmark where
  run (GraphDBClientSessionBenchmark socket a) n = do
    R.runGenT $ do
      gen <- ask
      replicateM_ n $ do
        liftIO $ BG.initDir
        BG.runPersistentSession $ do
          BG.serve socket $ do
            lift $ flip runReaderT gen $ BG.runClientSession socket $ a



