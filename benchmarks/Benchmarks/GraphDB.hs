module Benchmarks.GraphDB where

import Benchmarks.Prelude
import Benchmarks.Model
import Control.Lens
import qualified GraphDB as G
import qualified Benchmarks.Util.FileSystem as FS


-- Values
-------------------------

-- | Stores counters of UID generators.
type Catalogue = (UID Artist, UID Genre, UID Song)


-- Edges
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


-- Boilerplate
-------------------------

G.deriveSetup ''Catalogue
instance (Hashable a) => Hashable (UID a)
instance (Serializable m a) => Serializable m (UID a)


-- Interpreter
-------------------------

interpretSession :: 
  (G.Session s, u ~ Catalogue,
   MonadTrans (s u), MonadIO (s u m),
   MonadIO m, MonadBaseControl IO m) =>
  Session m a -> s u m a
interpretSession = iterTM $ \case
  InsertArtist a c -> G.write (insertArtist a) >>= c
  InsertGenre g c -> G.write (insertGenre g) >>= c
  InsertSong s gl al c -> G.write (insertSong s gl al) >>= c
  LookupArtistByUID u c -> G.read (lookupArtistByUID u) >>= c
  LookupArtistsByName n c -> G.read (lookupArtistsByName n) >>= c
  LookupArtistsBySongGenreName n c -> G.read (lookupArtistsBySongGenreName n) >>= c
  where
    
    lookupArtistByUID :: UID Artist -> G.Read s Catalogue t (Maybe (Identified Artist))
    lookupArtistByUID uid =
      G.getRoot >>= flip G.getTargetsByIndex (Catalogue_Artist_UID uid) >>=
      return . listToMaybe >>= mapM G.getValue

    lookupArtistsByName :: Text -> G.Read s Catalogue t [Identified Artist]
    lookupArtistsByName n = 
      G.getRoot >>= flip G.getTargetsByIndex (Catalogue_Artist_Name n) >>= mapM G.getValue

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

    generateUID :: Lens' Catalogue (UID a) -> G.Write s Catalogue t (UID a)
    generateUID selector = do
      root <- G.getRoot
      updateNode root $ zoom selector $ modify succ >> get

    updateNode :: 
      (G.PolyValue Catalogue v) => 
      G.Node s Catalogue t v -> State v r -> G.Write s Catalogue t r
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
  FS.removeIfExists dir
  FS.createTree dir

socketPath = dir <> ".socket"
dir = "./dist/benchmarks/graph-db"
initialRoot = (UID 0, UID 0, UID 0)
