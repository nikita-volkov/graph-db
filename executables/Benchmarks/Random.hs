module Benchmarks.Random where

import Benchmarks.Prelude
import qualified System.Random.MWC as MWC
import qualified Data.Char as Char

type Gen = MWC.Gen RealWorld
type GenT = ReaderT Gen

newGen :: IO Gen
newGen = MWC.create

runGenT :: MonadIO m => Gen -> GenT m r -> m r
runGenT gen t = runReaderT t gen

generateName :: MonadIO m => GenT m Text
generateName = do
  gen <- ask
  length <- liftIO $ MWC.uniformR lengthRange gen
  chars <- replicateM length $ generateChar
  return $! packText chars
  where
    lengthRange = (1, 50)

generateChar :: MonadIO m => GenT m Char
generateChar = do
  gen <- ask
  scenario :: Int <- liftIO $ MWC.uniformR (0, 6) gen
  ord <- liftIO $ flip MWC.uniformR gen $ case scenario of
    0 -> upperRange
    1 -> upperRange
    2 -> lowerRange
    3 -> lowerRange
    4 -> lowerRange
    5 -> lowerRange
    6 -> numRange
  return $! Char.chr ord
  where
    upperRange = (Char.ord 'A', Char.ord 'Z')
    lowerRange = (Char.ord 'a', Char.ord 'z')
    numRange = (Char.ord '0', Char.ord '9')

generateVariate :: (MonadIO m, MWC.Variate n) => (n, n) -> GenT m n
generateVariate r = ReaderT $ liftIO . MWC.uniformR r
