{-# LANGUAGE UndecidableInstances #-}
module Benchmarks.AcidState where

import Benchmarks.Prelude
import Control.Lens
import Benchmarks.Model
import qualified Benchmarks.Util.FileSystem as Fil
import qualified Data.IxSet as Ixs
import qualified Data.SafeCopy as Saf
import qualified Benchmarks.AcidState.AcidStatePlus as Aci

-- * Model Extensions
-------------------------

type Catalogue = 
  (
    IxSetWithUIDs Artist,
    IxSetWithUIDs Genre,
    IxSetWithUIDs Song
  )

-- | A basic extension of IxSet for support of UIDs.
type IxSetWithUIDs a = (NextUID a, Ixs.IxSet (Row a))
type NextUID a = UID a

data Row a = Row (UID a) a (Relations a)
deriving instance (Show (Relations a), Show a) => Show (Row a)
deriving instance (Eq (Relations a), Eq a) => Eq (Row a)
deriving instance (Ord (Relations a), Ord a) => Ord (Row a)
deriving instance (Generic (Relations a), Generic a) => Generic (Row a)
deriving instance (Data (Relations a), Data a) => Data (Row a)
deriving instance Typeable1 Row

type family Relations a
type instance Relations Artist = ()
type instance Relations Genre = ([UID Song])
type instance Relations Song = ([UID Artist])


-- * IxSet
-------------------------

instance Ixs.Indexable (Row Artist) where
  empty = Ixs.ixSet
    [
      Ixs.ixFun $ \(Row u (Artist n) _) -> [u],
      Ixs.ixFun $ \(Row u (Artist n) _) -> [n]
    ]

instance Ixs.Indexable (Row Genre) where
  empty = Ixs.ixSet
    [
      Ixs.ixFun $ \(Row u (Genre n) _) -> [u],
      Ixs.ixFun $ \(Row u (Genre n) _) -> [n]
    ]

instance Ixs.Indexable (Row Song) where
  empty = Ixs.ixSet
    [
      Ixs.ixFun $ \(Row u (Song n) _) -> [u],
      Ixs.ixFun $ \(Row u (Song n) _) -> [n]
    ]


-- * SafeCopy
-------------------------

instance (Saf.SafeCopy a, Saf.SafeCopy (Relations a)) => Saf.SafeCopy (Row a) where
  putCopy (Row arg_aadC arg_aadD arg_aadE) = Saf.contain $ do 
    safePut_UIDa_aadF <- Saf.getSafePut
    safePut_a_aadG <- Saf.getSafePut
    safePut_Relationsa_aadH <- Saf.getSafePut
    safePut_UIDa_aadF arg_aadC
    safePut_a_aadG arg_aadD
    safePut_Relationsa_aadH arg_aadE
    return ()
  getCopy = Saf.contain $ do
    safeGet_UIDa_aadI <- Saf.getSafeGet;
    safeGet_a_aadJ <- Saf.getSafeGet;
    safeGet_Relationsa_aadK <- Saf.getSafeGet;
    (((return Row) <*> safeGet_UIDa_aadI) <*> safeGet_a_aadJ) <*> safeGet_Relationsa_aadK
  version = 0
  kind = Saf.base
  errorTypeName _ = "Benchmarks.AcidState.Row"

Saf.deriveSafeCopy 0 'Saf.base ''UID
Saf.deriveSafeCopy 0 'Saf.base ''Identified
Saf.deriveSafeCopy 0 'Saf.base ''Artist
Saf.deriveSafeCopy 0 'Saf.base ''Genre
Saf.deriveSafeCopy 0 'Saf.base ''Song


-- * Acid
-------------------------

-- ** Events
-------------------------

insertArtistEvent :: Artist -> Aci.Update Catalogue (UID Artist)
insertArtistEvent artist = do
  liftState $ 
    zoom _1 $ do
      uid <- _1 <<%= succ
      _2 %= Ixs.insert (Row uid artist ())
      return uid

insertGenreEvent :: Genre -> [UID Song] -> Aci.Update Catalogue (UID Genre)
insertGenreEvent genre songUIDs = do
  liftState $ 
    zoom _2 $ do
      uid <- _1 <<%= succ
      _2 %= Ixs.insert (Row uid genre songUIDs)
      return uid

insertSongEvent :: Song -> [UID Artist] -> Aci.Update Catalogue (UID Song)
insertSongEvent song artistUIDs = do
  liftState $ 
    zoom _3 $ do
      uid <- _1 <<%= succ
      _2 %= Ixs.insert (Row uid song artistUIDs)
      return uid

-- | A helper, which makes \"lens\" functions usable on Acid-state monads.
liftState :: (MonadState s m) => State s r -> m r
liftState = state . runState


Aci.makeAcidic ''Catalogue ['insertArtistEvent, 'insertGenreEvent, 'insertSongEvent]


-- ** Setup
-------------------------


interpretSession :: (MonadIO m) => Session m r -> Aci.Session Catalogue m r
interpretSession = iterTM $ \case
  InsertArtist a c -> do
    r <- Aci.update $ InsertArtistEvent a  
    c r
  InsertGenre g c -> $notImplemented
  InsertSong s gl al c -> $notImplemented
  LookupArtistByUID u c -> $notImplemented
  LookupArtistsByName n c -> $notImplemented
  LookupArtistsBySongGenreName n c -> $notImplemented


data Settings =
  LocalPersistent |
  LocalNonpersistent

runSession :: (MonadIO m, MonadBaseControl IO m) => Settings -> Session m r -> m r
runSession = \case
  LocalPersistent -> Aci.runLocalPersistentSession dir initValue . interpretSession
  LocalNonpersistent -> Aci.runLocalNonpersistentSession dir initValue . interpretSession

dir = "./dist/benchmarks/acid-state"
initValue = ((UID 1, Ixs.empty), (UID 1, Ixs.empty), (UID 1, Ixs.empty))

initDir :: IO ()
initDir = do
  Fil.removeIfExists dir
  Fil.createTree dir
