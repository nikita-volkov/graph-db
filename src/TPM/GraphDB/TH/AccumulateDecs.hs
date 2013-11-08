module TPM.GraphDB.TH.AccumulateDecs where

import TPM.GraphDB.Prelude
import Language.Haskell.TH
import qualified Control.Concurrent.ParallelIO.Local as ParallelIO


-- | Accumulator of declarations capable of composing concurrent execution.
newtype AccumulateDecs r = AccumulateDecs (ReaderT ParallelIO.Pool (WriterT [Dec] Q) r)
  deriving (Functor, Applicative, Monad)

instance MonadIO AccumulateDecs where
  liftIO io = AccumulateDecs $ lift $ lift $ runIO io

liftDecsQ :: Q [Dec] -> AccumulateDecs ()
liftDecsQ decsQ = AccumulateDecs $ tell =<< (lift $ lift decsQ)

liftQ :: Q r -> AccumulateDecs r
liftQ q = AccumulateDecs $ lift $ lift q

run :: AccumulateDecs r -> Q (r, [Dec])
run (AccumulateDecs t) = 
  runIO $ do
    numCapabilities <- getNumCapabilities
    ParallelIO.withPool numCapabilities $ runQ . runWriterT . runReaderT t

exec :: AccumulateDecs r -> Q [Dec]
exec = fmap snd . run

-- | Same as 'sequence_', but does this concurrently.
sequenceConcurrently_ :: [AccumulateDecs a] -> AccumulateDecs ()
sequenceConcurrently_ actions = AccumulateDecs $ do
  pool <- ask
  results <- lift $ lift $ runIO $ 
    ParallelIO.parallelInterleaved pool $ map (actionToIO pool) actions 
  tell $ join results
  where
    actionToIO pool (AccumulateDecs t) = runQ $ execWriterT $ runReaderT t pool

