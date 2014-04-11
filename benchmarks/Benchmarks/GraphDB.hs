module Benchmarks.GraphDB where

import Benchmarks.Prelude
import qualified GraphDB as G


-- Values
-------------------------

data Catalogue = Catalogue UID deriving (Show, Eq, Generic)
data Artist = Artist Name deriving (Show, Eq, Generic)
data Genre = Genre Name deriving (Show, Eq, Generic)
data Song = Song Name deriving (Show, Eq, Generic)
type UID = Int
type Name = Text
type Identified a = (UID, a)


-- Edges
-------------------------

instance G.Edge Catalogue (Identified Artist) where
  data Index Catalogue (Identified Artist) =
    Catalogue_Artist_UID UID |
    Catalogue_Artist_Name Text
    deriving (Eq, Generic)
  indexes (uid, Artist n) = 
    [Catalogue_Artist_UID uid, Catalogue_Artist_Name n]

instance G.Edge Catalogue (Identified Genre) where
  data Index Catalogue (Identified Genre) = 
    Catalogue_Genre_UID UID |
    Catalogue_Genre_Name Text
    deriving (Eq, Generic)
  indexes (uid, Genre n) = 
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


-- Transactions
-------------------------

-- | A simple query.
getArtistsByName :: Text -> G.Read s Catalogue t [Identified Artist]
getArtistsByName n = 
  G.getRoot >>= flip G.getTargetsByIndex (Catalogue_Artist_Name n) >>= mapM G.getValue

-- | A deep query.
getArtistsBySongGenreName :: Text -> G.Read s Catalogue t [Identified Artist]
getArtistsBySongGenreName n =
  G.getRoot >>= 
  flip G.getTargetsByIndex (Catalogue_Genre_Name n) >>=
  mapM (flip G.getTargetsByIndex Genre_Song) >>= 
  return . concat >>=
  mapM (flip G.getTargetsByIndex Song_Artist) >>=
  return . concat >>=
  mapM G.getValue

insertArtist :: Artist -> G.Write s Catalogue t (Identified Artist)
insertArtist value = do
  root <- G.getRoot
  identified <- (,) <$> generateNewUID <*> pure value
  node <- G.newNode identified
  G.addTarget root node
  return identified

-- | Use a counter stored in the root 'Catalogue' node to generate a new unique UID.
generateNewUID :: G.Write s Catalogue t UID
generateNewUID = do
  root <- G.getRoot
  Catalogue lastUID <- G.getValue root
  let newUID = lastUID + 1
  G.setValue root (Catalogue newUID)
  return newUID


-- Setup
-------------------------

runPersistedSession :: G.PersistentSession Catalogue m r -> m r
runPersistedSession = $notImplemented

runNonpersistedSession = $notImplemented

serve socket = $notImplemented

runClientSession socket = $notImplemented
