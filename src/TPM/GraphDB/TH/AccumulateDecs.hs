module TPM.GraphDB.TH.AccumulateDecs where

import TPM.GraphDB.Prelude
import Language.Haskell.TH
import qualified TPM.GraphDB.CIO as CIO; import TPM.GraphDB.CIO (CIO)

-- | Accumulator of declarations capable of composing concurrent execution.
newtype AccumulateDecs r = AccumulateDecs (WriterT [Dec] CIO r)
  deriving (Functor, Applicative, Monad)

instance MonadIO AccumulateDecs where
  liftIO = AccumulateDecs . liftIO

liftQ :: Q r -> AccumulateDecs r
liftQ = AccumulateDecs . liftIO . runQ

liftDecsQ :: Q [Dec] -> AccumulateDecs ()
liftDecsQ decsQ = AccumulateDecs . tell =<< liftQ decsQ

run :: AccumulateDecs r -> Q (r, [Dec])
run (AccumulateDecs t) = runIO $ CIO.run' $ runWriterT t

exec :: AccumulateDecs r -> Q [Dec]
exec = fmap snd . run

-- | Same as @Control.Monad.'Control.Monad.sequence_'@, but does it concurrently. 
-- Blocks the calling thread until all actions are finished.
sequence_ :: [AccumulateDecs a] -> AccumulateDecs ()
sequence_ actions = AccumulateDecs $ do
  results <- lift $ CIO.sequenceInterleaved $ map actionToCIO actions
  tell $ join results
  where
    actionToCIO (AccumulateDecs t) = execWriterT t

