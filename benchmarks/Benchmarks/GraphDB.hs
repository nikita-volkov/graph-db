module Benchmarks.GraphDB where

import Benchmarks.Prelude
import Control.Lens
import qualified GraphDB as G
import qualified Benchmarks.Util.FileSystem as FS


-- Values
-------------------------

-- | Stores counters of UID generators.
type Catalogue = (UID Artist, UID Genre, UID Song)
newtype UID a = UID Int deriving (Show, Eq, Generic, Ord, Enum, Num, Real, Integral)
data Artist = Artist Name deriving (Show, Eq, Generic)
data Genre = Genre Name deriving (Show, Eq, Generic)
data Song = Song Name deriving (Show, Eq, Generic)
type Name = Text
type Identified a = (UID a, a)


-- Edges
-------------------------

instance G.Edge Catalogue (Identified Artist) where
  data Index Catalogue (Identified Artist) =
    Catalogue_Artist_UID (UID Artist) |
    Catalogue_Artist_Name Text
    deriving (Eq, Generic)
  indexes (uid, Artist n) = 
    [Catalogue_Artist_UID uid, Catalogue_Artist_Name n]

instance G.Edge Catalogue (Identified Genre) where
  data Index Catalogue (Identified Genre) = 
    Catalogue_Genre_UID (UID Genre) |
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
instance (Hashable a) => Hashable (UID a)
instance (Serializable m a) => Serializable m (UID a)


-- Transactions
-------------------------

-- | A query by UID.
getArtistByUID :: UID Artist -> G.Read s Catalogue t (Maybe (Identified Artist))
getArtistByUID uid =
  G.getRoot >>= flip G.getTargetsByIndex (Catalogue_Artist_UID uid) >>=
  return . listToMaybe >>= mapM G.getValue

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

insertArtist :: Artist -> G.Write s Catalogue t (UID Artist)
insertArtist value = do
  root <- G.getRoot
  uid <- updateNode root $ zoom _1 $ modify succ >> get
  node <- G.newNode (uid, value)
  G.addTarget root node
  return uid

insertGenre :: Genre -> G.Write s Catalogue t (UID Genre)
insertGenre value = do
  root <- G.getRoot
  uid <- updateNode root $ zoom _2 $ modify succ >> get
  node <- G.newNode (uid, value)
  G.addTarget root node
  return uid

insertSong :: Song -> [UID Genre] -> [UID Artist] -> G.Write s Catalogue t (UID Song)
insertSong value genreUIDs artistUIDs = do
  root <- G.getRoot
  uid <- updateNode root $ zoom _3 $ modify succ >> get
  node <- G.newNode (uid, value)
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


-- Setup
-------------------------

runPersistentSession ::   
  (MonadBaseControl IO m, MonadIO m) =>
  G.PersistentSession Catalogue m r -> m (Either G.PersistenceFailure r)
runPersistentSession = G.runPersistentSession (initialRoot, dir, buffering) where
  buffering = 100

runNonpersistentSession :: 
  (MonadIO m) => G.NonpersistentSession Catalogue m r -> m r
runNonpersistentSession = G.runNonpersistentSession initialRoot

serve socket = G.serve (1, lm, to, mc, log) where
  lm = if socket 
    then G.ListeningMode_Socket socketPath
    else G.ListeningMode_Host 54699 auth 
    where
      auth = const $ return True
  to = 10^6
  mc = 100
  log = const $ return ()

runClientSession socket = G.runClientSession (1, url) where
  url = if socket
    then G.URL_Socket socketPath
    else G.URL_Host "127.0.0.1" 54699 Nothing

initDir :: IO ()
initDir = do
  FS.remove dir

socketPath = dir <> ".socket"
dir = "./dist/benchmarks/db"
initialRoot = (UID 0, UID 0, UID 0)
