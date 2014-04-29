module APITests.Catalogue where

import APITests.Prelude
import Control.Lens
import qualified GraphDB as G
import qualified Test.QuickCheck as Q hiding (oneof, listOf, elements, choose)
import qualified QuickCheck.GenT as Q
import qualified Data.Vector as V
import Test.QuickCheck.Instances ()



-- * Model
-------------------------

-- | 
-- The root node. 
-- For convenience it stores the counters of UID generators.
type Catalogue = (UID Artist, UID Genre, UID Song)
newtype UID a = UID Int deriving (Show, Eq, Ord, Generic, Data, Typeable, Enum, Num, Real, Integral)
data Artist = Artist Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Genre = Genre Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Song = Song Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
type Name = Text
data Identified a = Identified {-# UNPACK #-} !(UID a) !a deriving (Show, Eq, Ord, Generic, Data, Typeable)

initRoot :: Catalogue = (0, 0, 0)

instance Functor Identified where
  fmap f (Identified (UID n) a) = Identified (UID n) (f a)


-- * Edges
-------------------------

instance G.Edge Catalogue (Identified Artist) where
  data Index Catalogue (Identified Artist) =
    Catalogue_Artist_UID (UID Artist) |
    Catalogue_Artist_Name Text |
    Catalogue_Artist
    deriving (Eq, Generic)
  indexes (Identified uid (Artist n)) = 
    [Catalogue_Artist_UID uid, Catalogue_Artist_Name n, Catalogue_Artist]

instance G.Edge Catalogue (Identified Genre) where
  data Index Catalogue (Identified Genre) = 
    Catalogue_Genre_UID (UID Genre) |
    Catalogue_Genre_Name Text |
    Catalogue_Genre
    deriving (Eq, Generic)
  indexes (Identified uid (Genre n)) = 
    [Catalogue_Genre_UID uid, Catalogue_Genre_Name n, Catalogue_Genre]

instance G.Edge Catalogue (Identified Song) where
  data Index Catalogue (Identified Song) = 
    Catalogue_Song_UID (UID Song) |
    Catalogue_Song_Name Text |
    Catalogue_Song
    deriving (Eq, Generic)
  indexes (Identified uid (Song n)) = 
    [Catalogue_Song_UID uid, Catalogue_Song_Name n, Catalogue_Song]

instance G.Edge (Identified Genre) (Identified Song) where
  data Index (Identified Genre) (Identified Song) =
    Genre_Song
    deriving (Eq, Generic)
  indexes _ = [Genre_Song]

instance G.Edge (Identified Song) (Identified Artist) where
  data Index (Identified Song) (Identified Artist) =
    Song_Artist
    deriving (Eq, Generic)
  indexes _ = [Song_Artist]


-- * Default Transactions
-------------------------

lookupArtistByUID :: UID Artist -> G.Read s Catalogue t (Maybe (Identified Artist))
lookupArtistByUID uid =
  G.getRoot >>= flip G.getTargets (Catalogue_Artist_UID uid) >>=
  return . listToMaybe >>= mapM G.getValue

lookupArtistsByName :: Text -> G.Read s Catalogue t [Identified Artist]
lookupArtistsByName n = 
  G.getRoot >>= flip G.getTargets (Catalogue_Artist_Name n) >>= mapM G.getValue

lookupArtistsBySongGenreName :: Text -> G.Read s Catalogue t [Identified Artist]
lookupArtistsBySongGenreName n =
  G.getRoot >>= 
  flip G.getTargets (Catalogue_Genre_Name n) >>=
  mapM (flip G.getTargets Genre_Song) >>= 
  return . concat >>=
  mapM (flip G.getTargets Song_Artist) >>=
  return . concat >>=
  mapM G.getValue

insertArtist :: Artist -> G.Write s Catalogue t (UID Artist)
insertArtist value = do
  root <- G.getRoot
  uid <- updateNode root $ zoom _1 $ modify succ >> get
  node <- G.newNode (Identified uid value)
  G.addTarget root node
  return uid

insertGenre :: Genre -> G.Write s Catalogue t (UID Genre)
insertGenre value = do
  root <- G.getRoot
  uid <- updateNode root $ zoom _2 $ modify succ >> get
  node <- G.newNode (Identified uid value)
  G.addTarget root node
  return uid

insertSong :: Song -> [UID Genre] -> [UID Artist] -> G.Write s Catalogue t (UID Song)
insertSong value genreUIDs artistUIDs = do
  root <- G.getRoot
  uid <- updateNode root $ zoom _3 $ modify succ >> get
  node <- G.newNode (Identified uid value)
  forM_ genreUIDs $ \uid -> do
    genres <- G.getTargets root (Catalogue_Genre_UID uid)
    forM_ genres $ \genre -> do
      G.addTarget genre node
  forM_ artistUIDs $ \uid -> do
    artists <- G.getTargets root (Catalogue_Artist_UID uid)
    forM_ artists $ \artist -> do
      G.addTarget node artist
  return uid

generateUID :: Lens' Catalogue (UID a) -> G.Write s Catalogue t (UID a)
generateUID selector = do
  root <- G.getRoot
  updateNode root $ zoom selector $ modify succ >> get

updateNode :: 
  (G.PolyValue Catalogue v) => 
  G.Node s Catalogue t v -> State v r -> G.Write s Catalogue t r
updateNode n u = G.getValue n >>= return . runState u >>= \(r, v') -> G.setValue n v' >> return r


-- * QuickCheck
-------------------------

newtype Update s t = Update (G.Write s Catalogue t ())

instance Show (Update s t) where
  show _ = "Update"

instance Q.Arbitrary (Update s t) where
  arbitrary = fmap Update $ Q.runGenT $ Q.oneof $ 
    replicate 200 insertSomeArtist ++
    replicate 2000 insertSomeSong ++
    replicate 30 insertSomeGenre ++
    replicate 10 removeSomeArtist ++
    replicate 100 removeSomeSong ++
    replicate 1 removeSomeGenre
    where
      insertSomeArtist = do
        name <- Q.liftGen $ Q.arbitrary
        lift $ void $ insertArtist $ Artist name
      insertSomeGenre = do
        songs <- chooseSomeSongs
        name <- Q.liftGen $ Q.arbitrary
        lift $ do
          root <- G.getRoot
          uid <- generateUID _2
          genre <- G.newNode $ Identified uid $ Genre name
          G.addTarget root genre
          forM_ songs $ \song -> do
            G.addTarget genre song
        where
          chooseSomeSongs = do
            list <- lift $ G.getRoot >>= flip G.getTargets Catalogue_Song
            let vec = V.fromList list
                length = V.length vec
            amount <- if length > 0 
              then Q.choose (0, div length 10) 
              else return 0
            replicateM amount $ Q.choose (0, length - 1) >>= return . V.unsafeIndex vec
      insertSomeSong = do
        artists <- chooseSomeArtists
        name <- Q.liftGen $ Q.arbitrary
        lift $ do
          root <- G.getRoot
          uid <- generateUID _3
          song <- G.newNode $ Identified uid $ Song name
          G.addTarget root song
          forM_ artists $ \artist -> do
            G.addTarget song artist
        where
          chooseSomeArtists = do
            list <- lift $ G.getRoot >>= flip G.getTargets Catalogue_Artist
            let vec = V.fromList list
                length = V.length vec
            amount <- if length > 0 
              then Q.elements $ replicate 100 1 ++ replicate 10 2 ++ replicate 2 3
              else return 0
            replicateM amount $ Q.choose (0, length - 1) >>= return . V.unsafeIndex vec
      removeSomeArtist = do
        root <- lift $ G.getRoot
        all <- lift $ G.getTargets root Catalogue_Artist
        mapM_ (lift . G.remove) =<< Q.elementsMay all
      removeSomeSong = do
        root <- lift $ G.getRoot
        all <- lift $ G.getTargets root Catalogue_Song
        mapM_ (lift . G.remove) =<< Q.elementsMay all
      removeSomeGenre = do
        root <- lift $ G.getRoot
        all <- lift $ G.getTargets root Catalogue_Genre
        mapM_ (lift . G.remove) =<< Q.elementsMay all


-- * Boilerplate
-------------------------

G.deriveSetup ''Catalogue
instance (Hashable a) => Hashable (UID a)
instance (Serializable m a) => Serializable m (UID a)


