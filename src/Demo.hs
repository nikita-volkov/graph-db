
import GraphDB.Prelude
import qualified GraphDB as DB



main = do
  db :: DB.Engine Catalogue <- 
    DB.startEngine =<< return . DB.Mode_Local . Just . (100,) =<< DB.pathsFromDirectory dir
  DB.runEvent db $ InsertArtist (Artist "Metallica") [Genre "Metal", Genre "Rock"]
  DB.runEvent db $ InsertArtist (Artist "Dire Straits") [Genre "Rock"]

  metallicaGenres <- DB.runEvent db $ GetGenresByArtistName "Metallica"
  direStraitsGenres <- DB.runEvent db $ GetGenresByArtistName "Dire Straits"
  putStrLn $ "Metallica's genres: " ++ show metallicaGenres
  putStrLn $ "Dire Straits' genres: " ++ show direStraitsGenres
  DB.shutdownEngine db
  where
    dir = "./dist/demo/"

insertArtist :: Artist -> [Genre] -> DB.Write Catalogue s ()
insertArtist artist genreList = do
  artistRef <- DB.newNode artist
  rootRef <- DB.getRoot
  DB.insertEdge rootRef ArtistOf artistRef
  DB.insertEdge rootRef (ArtistOfByName (artistName artist)) artistRef
  -- Lookup existing genres and insert inexistent ones, then insert appropriate edges.
  for_ genreList $ \genre -> do
    genreRefs <- do
      existing <- DB.getTargets (GenreOfByGenre genre) rootRef
      case existing of
        [] -> insertGenreAndGetRef genre >>= return . (:[])
        _ -> return existing
    for_ genreRefs $ \genreRef -> DB.insertEdge artistRef GenreOf genreRef

-- | 
-- Since this function returns a 'DB.NodeRef' it can't be directly used with 'DB.Event',
-- instead it can only be used in composition of others, like in 'insertArtist'.
insertGenreAndGetRef :: Genre -> DB.Write Catalogue s (DB.NodeRef Catalogue s Genre)
insertGenreAndGetRef genre = do
  -- O(1):
  new <- DB.newNode genre
  -- O(1):
  root <- DB.getRoot
  -- O(log n), where "n" is the number of edges from root node:
  DB.insertEdge root (GenreOfByGenre genre) new
  -- O(log n), where "n" is the number of edges from root node:
  DB.insertEdge root GenreOf new
  return new

getGenresByArtistName :: Text -> DB.Read Catalogue s [Genre]
getGenresByArtistName name = 
  -- O(1):
  DB.getRoot >>=
  -- O(log n), where "n" is the number of edges from root node:
  DB.getTargets (ArtistOfByName name) >>=
  -- O(log n * m), where "n" is the number of edges from target node and 
  -- "m" is the number of target nodes:
  traverse (DB.getTargets GenreOf) >>=
  return . concat >>=
  -- O(n), where "n" is the number of result nodes, 
  -- i.e. the complexity of operation "getValue" is O(1):
  traverse DB.getValue



data Catalogue
data Artist = Artist {artistName :: Text} deriving (Show)
data Genre = Genre {genreName :: Text} deriving (Show)
data instance DB.Edge Artist = ArtistOf | ArtistOfByName Text
data instance DB.Edge Genre = GenreOf | GenreOfByGenre Genre



--------------------------
-- The Boilerplate
--------------------------
-- Everything following should be eliminated when TemplateHaskell macros get implemented.
-- 

instance DB.Tag Catalogue where
  data MemberValue Catalogue =
    MemberValue_Unit () | 
    MemberValue_Artist Artist | 
    MemberValue_Genre Genre
  data MemberEdge Catalogue =
    MemberEdge_Artist (DB.Edge Artist) |
    MemberEdge_Genre (DB.Edge Genre)
  data MemberEvent Catalogue = 
    MemberEvent_InsertArtist InsertArtist |
    MemberEvent_GetGenresByArtistName GetGenresByArtistName
  data MemberEventResult Catalogue = 
    MemberEventResult_Unit () |
    MemberEventResult_Genres [Genre]
  memberEventTransaction memberEvent = case memberEvent of
    MemberEvent_InsertArtist e -> MemberEventResult_Unit <$> DB.eventTransaction e
    MemberEvent_GetGenresByArtistName e -> MemberEventResult_Genres <$> DB.eventTransaction e

instance DB.ValueOf () Catalogue where
  toMemberValue = MemberValue_Unit
  fromMemberValue (MemberValue_Unit z) = Just z
  fromMemberValue _ = Nothing

instance DB.ValueOf Artist Catalogue where
  toMemberValue = MemberValue_Artist
  fromMemberValue (MemberValue_Artist z) = Just z
  fromMemberValue _ = Nothing

instance DB.ValueOf Genre Catalogue where
  toMemberValue = MemberValue_Genre
  fromMemberValue (MemberValue_Genre z) = Just z
  fromMemberValue _ = Nothing

instance DB.EdgeOf (DB.Edge Artist) Catalogue where
  toMemberEdge = MemberEdge_Artist
  fromMemberEdge (MemberEdge_Artist z) = Just z
  fromMemberEdge _ = Nothing

instance DB.EdgeOf (DB.Edge Genre) Catalogue where
  toMemberEdge = MemberEdge_Genre
  fromMemberEdge (MemberEdge_Genre z) = Just z
  fromMemberEdge _ = Nothing


data InsertArtist = InsertArtist Artist [Genre]

instance DB.EventOf InsertArtist Catalogue where
  type EventResult InsertArtist Catalogue = ()
  eventTransaction (InsertArtist artist genres) = 
    DB.Write $ insertArtist artist genres
  toMemberEvent = MemberEvent_InsertArtist
  fromMemberEvent (MemberEvent_InsertArtist z) = Just z
  fromMemberEvent _ = Nothing

instance DB.EventResultOf () Catalogue where
  toMemberEventResult = MemberEventResult_Unit
  fromMemberEventResult (MemberEventResult_Unit z) = Just z
  fromMemberEventResult _ = Nothing

data GetGenresByArtistName = GetGenresByArtistName Text

instance DB.EventOf GetGenresByArtistName Catalogue where
  type EventResult GetGenresByArtistName Catalogue = [Genre]
  eventTransaction (GetGenresByArtistName name) = 
    DB.Read $ getGenresByArtistName name
  toMemberEvent = MemberEvent_GetGenresByArtistName
  fromMemberEvent (MemberEvent_GetGenresByArtistName z) = Just z
  fromMemberEvent _ = Nothing  

instance DB.EventResultOf [Genre] Catalogue where
  toMemberEventResult = MemberEventResult_Genres
  fromMemberEventResult (MemberEventResult_Genres z) = Just z
  fromMemberEventResult _ = Nothing

-- 



deriving instance Eq Artist
deriving instance Eq Genre
deriving instance Eq (DB.Edge Genre)
deriving instance Eq (DB.Edge Artist)
deriving instance Eq (DB.MemberEdge Catalogue)
deriving instance Eq (DB.MemberValue Catalogue)
deriving instance Generic Artist
deriving instance Generic Genre
deriving instance Generic (DB.Edge Genre)
deriving instance Generic (DB.Edge Artist)
deriving instance Generic (DB.MemberEdge Catalogue)
deriving instance Generic (DB.MemberValue Catalogue)
deriving instance Generic (DB.MemberEvent Catalogue)
deriving instance Generic (DB.MemberEventResult Catalogue)
deriving instance Generic (InsertArtist)
deriving instance Generic (GetGenresByArtistName)
instance Hashable Artist
instance Hashable Genre
instance Hashable (DB.Edge Genre)
instance Hashable (DB.Edge Artist)
instance Hashable (DB.MemberEdge Catalogue)
instance Hashable (DB.MemberValue Catalogue)

instance Serializable IO Artist
instance Serializable IO Genre
instance Serializable IO (DB.MemberValue Catalogue)
instance Serializable IO (DB.Edge Genre)
instance Serializable IO (DB.Edge Artist)
instance Serializable IO (DB.MemberEdge Catalogue)
instance Serializable IO InsertArtist
instance Serializable IO GetGenresByArtistName
instance Serializable IO (DB.MemberEvent Catalogue)
instance Serializable IO (DB.MemberEventResult Catalogue)
