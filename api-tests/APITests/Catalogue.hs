module APITests.Catalogue where

import APITests.Prelude
import Control.Lens
import qualified GraphDB as G
import qualified Test.QuickCheck as QC hiding (oneof, listOf, elements)
import qualified QuickCheck.GenT as QC
import Test.QuickCheck.Instances ()


-- * Values
-------------------------

-- | Stores counters of UID generators.
type Catalogue = (UID Artist, UID Genre, UID Song)
newtype UID a = UID Int deriving (Show, Eq, Generic, Ord, Enum, Num, Real, Integral)
data Artist = Artist Name deriving (Show, Eq, Generic)
data Genre = Genre Name deriving (Show, Eq, Generic)
data Song = Song Name deriving (Show, Eq, Generic)
type Name = Text
data Identified a = Identified {-# UNPACK #-} !(UID a) !a deriving (Show, Eq, Generic)

initRoot :: Catalogue
initRoot = (UID 0, UID 0, UID 0)

-- * Edges
-------------------------

instance G.Edge Catalogue (Identified Artist) where
  data Index Catalogue (Identified Artist) =
    Catalogue_Artist_UID (UID Artist) |
    Catalogue_Artist_Name Text
    deriving (Eq, Generic)
  indexes (Identified uid (Artist n)) = 
    [Catalogue_Artist_UID uid, Catalogue_Artist_Name n]

instance G.Edge Catalogue (Identified Genre) where
  data Index Catalogue (Identified Genre) = 
    Catalogue_Genre_UID (UID Genre) |
    Catalogue_Genre_Name Text
    deriving (Eq, Generic)
  indexes (Identified uid (Genre n)) = 
    [Catalogue_Genre_UID uid, Catalogue_Genre_Name n]

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


-- Boilerplate
-------------------------

G.deriveUnion ''Catalogue
instance (Hashable a) => Hashable (UID a)
instance (Serializable m a) => Serializable m (UID a)


-- * Transactions
-------------------------

-- | A query by UID.
lookupArtistByUID :: UID Artist -> G.Read s Catalogue t (Maybe (Identified Artist))
lookupArtistByUID uid =
  G.getRoot >>= flip G.getTargetsByIndex (Catalogue_Artist_UID uid) >>=
  return . listToMaybe >>= mapM G.getValue

-- | A simple query.
lookupArtistsByName :: Text -> G.Read s Catalogue t [Identified Artist]
lookupArtistsByName n = 
  G.getRoot >>= flip G.getTargetsByIndex (Catalogue_Artist_Name n) >>= mapM G.getValue

-- | A deep query.
lookupArtistsBySongGenreName :: Text -> G.Read s Catalogue t [Identified Artist]
lookupArtistsBySongGenreName n =
  G.getRoot >>= 
  flip G.getTargetsByIndex (Catalogue_Genre_Name n) >>=
  mapM (flip G.getTargetsByIndex Genre_Song) >>= 
  return . concat >>=
  mapM (flip G.getTargetsByIndex Song_Artist) >>=
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
    genres <- G.getTargetsByIndex root (Catalogue_Genre_UID uid)
    forM_ genres $ \genre -> do
      G.addTarget genre node
  forM_ artistUIDs $ \uid -> do
    artists <- G.getTargetsByIndex root (Catalogue_Artist_UID uid)
    forM_ artists $ \artist -> do
      G.addTarget node artist
  return uid

updateNode :: G.Node s Catalogue t Catalogue -> State Catalogue r -> G.Write s Catalogue t r
updateNode n u = G.getValue n >>= return . runState u >>= \(r, v') -> G.setValue n v' >> return r

