module GraphDB.Util.Prelude 
  ( 
    module Exports,

    LazyByteString,
    LazyText,

    traceM,
    traceIO,
    traceIOWithTime,
    packText,
    unpackText,
    bug,
    (|>),
    (<|),
    (|$>),
    bracketME,
    finallyME,
    tracingExceptions,
    asyncRethrowing,
  )
  where

-- base
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, FilePath, id, (.))
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Applicative as Exports
import Control.Arrow as Exports hiding (left, right)
import Control.Category as Exports
import Data.Monoid as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports hiding (for)
import Data.Maybe as Exports
import Data.Either as Exports hiding (isLeft, isRight)
import Data.List as Exports hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Tuple as Exports
import Data.Ord as Exports (Down(..))
import Data.String as Exports
import Data.Int as Exports
import Data.Word as Exports
import Data.Ratio as Exports
import Data.Fixed as Exports
import Data.Ix as Exports
import Data.Data as Exports hiding (Proxy)
import Text.Read as Exports (readMaybe, readEither)
import Control.Exception as Exports hiding (tryJust, assert)
import Control.Concurrent as Exports hiding (yield)
import System.Mem.StableName as Exports
import System.Timeout as Exports
import System.Exit as Exports
import System.IO.Unsafe as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import Unsafe.Coerce as Exports
import GHC.Exts as Exports hiding (Any, traceEvent, toList)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Data.IORef as Exports
import Data.STRef as Exports
import Control.Monad.ST as Exports
import Debug.Trace as Exports hiding (traceIO, traceM)

-- mtl
import Control.Monad.Identity as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.State.Strict as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Reader as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Writer.Strict as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM, Any)
import Control.Monad.RWS.Strict as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM, Any)
import Control.Monad.Error as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Trans as Exports

-- transformers-base
import Control.Monad.Base as Exports

-- monad-control
import Control.Monad.Trans.Control as Exports

-- free
import Control.Monad.Trans.Free as Exports
import Control.Monad.Free.TH as Exports

-- stm
import Control.Concurrent.STM as Exports

-- lifted-async
import Control.Concurrent.Async.Lifted as Exports

-- bytestring
import Data.ByteString as Exports (ByteString)

-- text
import Data.Text as Exports (Text)

-- containers
import Data.Map as Exports (Map)
import Data.IntMap as Exports (IntMap)
import Data.Set as Exports (Set)
import Data.IntSet as Exports (IntSet)
import Data.Sequence as Exports (Seq)
import Data.Tree as Exports (Tree)

-- system-filepath
import Filesystem.Path as Exports (FilePath)

-- hashable
import Data.Hashable as Exports (Hashable(..), hash)

-- time
import Data.Time.Clock as Exports

-- pipes
import Pipes as Exports

-- pipes-cereal-plus
import PipesCerealPlus as Exports

-- either
import Control.Error as Exports

-- placeholders
import Development.Placeholders as Exports

import qualified Debug.Trace.LocationTH
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text
import qualified Prelude
import qualified Debug.Trace
import qualified System.Locale
import qualified Data.Time
import qualified Control.Concurrent.Async

type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text


(|>) :: a -> (a -> b) -> b
a |> aToB = aToB a
{-# INLINE (|>) #-}

(<|) :: (a -> b) -> a -> b
aToB <| a = aToB a
{-# INLINE (<|) #-}

-- | 
-- The following are all the same:
-- fmap f a == f <$> a == a |> fmap f == a |$> f
-- 
-- This operator accomodates the left-to-right operators: >>=, >>>, |>.
(|$>) = flip fmap
{-# INLINE (|$>) #-}

packText = Data.Text.pack
unpackText = Data.Text.unpack

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"graph-db\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

bracketME :: (MonadError e m) => m a -> (a -> m b) -> (a -> m c) -> m c
bracketME acquire release apply = do
  r <- acquire
  z <- catchError (liftM Right $ apply r) (return . Left)
  release r
  either throwError return z

finallyME :: (MonadError e m) => m a -> m b -> m a
finallyME m f = do
  z <- catchError (liftM Right $ m) (return . Left)
  f
  either throwError return z

traceM :: (Monad m) => String -> m ()
traceM s = trace s $ return ()

traceIO :: (MonadIO m) => String -> m ()
traceIO = liftIO . Debug.Trace.traceIO

traceIOWithTime :: (MonadIO m) => String -> m ()
traceIOWithTime s = do
  time <- liftIO $ getCurrentTime
  traceIO $ 
    formatTime time <> ": " <> s
  where
    formatTime = 
      take 15 . 
      Data.Time.formatTime System.Locale.defaultTimeLocale "%X.%q"

tracingExceptions :: (MonadBaseControl IO m) => m a -> m a
tracingExceptions m = 
  control $ \runInIO -> catch (runInIO m) $ \(SomeException e) -> do
    let rep = typeOf e
        tyCon = typeRepTyCon rep
    traceIOWithTime $ 
      "Uncaught exception: " ++ show e ++ "\n" ++
      "                 Type: " ++ show rep ++ "\n" ++
      "                 Module: " ++ tyConModule tyCon ++ "\n" ++
      "                 Package: " ++ tyConPackage tyCon
    throwIO $ e


-- Async
-------------------------

asyncRethrowing :: MonadBaseControl IO m => m a -> m (Async (StM m a))
asyncRethrowing m = 
  liftBaseWith $ \runInIO -> do
    parentTID <- myThreadId
    Control.Concurrent.Async.async $ do
      catch (runInIO m) $ \case
        se -> if
          | Just ThreadKilled <- fromException se -> liftBase $ throwIO ThreadKilled
          | otherwise -> throwTo parentTID se >> return undefined
