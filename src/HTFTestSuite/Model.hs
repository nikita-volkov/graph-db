module HTFTestSuite.Model where

import GraphDB.Prelude
import qualified GraphDB as DB


insertArtist :: Artist -> [Genre] -> DB.Write Catalogue s ()
insertArtist artist genreList = do
  artistRef <- DB.newNode artist
  rootRef <- DB.getRoot
  DB.insertEdge rootRef UnitToArtistEdge artistRef
  DB.insertEdge rootRef (UnitToArtistByNameEdge (artistName artist)) artistRef
  -- Lookup existing genres and insert inexistent ones, then insert appropriate edges.
  for_ genreList $ \genre -> do
    genreRefs <- do
      existing <- DB.getTargets (UnitToGenreByGenreEdge genre) rootRef
      case existing of
        [] -> insertGenreAndGetRef genre >>= return . (:[])
        _ -> return existing
    for_ genreRefs $ \genreRef -> DB.insertEdge artistRef ArtistToGenreEdge genreRef

-- | 
-- Since this function returns a 'DB.NodeRef' it can't be directly used with 'DB.Event',
-- instead it can only be used in composition of others, like in 'insertArtist'.
insertGenreAndGetRef :: Genre -> DB.Write Catalogue s (DB.NodeRef Catalogue s Genre)
insertGenreAndGetRef genre = do
  new <- DB.newNode genre
  root <- DB.getRoot
  DB.insertEdge root (UnitToGenreByGenreEdge genre) new
  DB.insertEdge root UnitToGenreEdge new
  return new

getGenresByArtistName :: Text -> DB.Read Catalogue s [Genre]
getGenresByArtistName name = 
  DB.getRoot >>=
  DB.getTargets (UnitToArtistByNameEdge name) >>=
  traverse (DB.getTargets ArtistToGenreEdge) >>=
  return . concat >>=
  traverse DB.getValue



data Catalogue
data Artist = Artist {artistName :: Text} deriving (Show)
data Genre = Genre {genreName :: Text} deriving (Show)
data instance DB.Edge () Artist = UnitToArtistEdge | UnitToArtistByNameEdge Text
data instance DB.Edge Artist Genre = ArtistToGenreEdge
data instance DB.Edge () Genre = UnitToGenreEdge | UnitToGenreByGenreEdge Genre



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
    MemberEdge_UnitToArtist (DB.Edge () Artist) |
    MemberEdge_UnitToGenre (DB.Edge () Genre) |
    MemberEdge_ArtistToGenre (DB.Edge Artist Genre)
  data MemberEvent Catalogue = 
    MemberEvent_InsertArtist InsertArtist |
    MemberEvent_GetGenresByArtistName GetGenresByArtistName
  data MemberEventResult Catalogue = 
    MemberEventResult_Unit () |
    MemberEventResult_Genres [Genre]
  memberEventTransaction memberEvent = case memberEvent of
    MemberEvent_InsertArtist e -> MemberEventResult_Unit <$> DB.eventTransaction e
    MemberEvent_GetGenresByArtistName e -> MemberEventResult_Genres <$> DB.eventTransaction e

instance DB.IsMemberValueOf () Catalogue where
  toMemberValue = MemberValue_Unit
  fromMemberValue (MemberValue_Unit z) = Just z
  fromMemberValue _ = Nothing

instance DB.IsMemberValueOf Artist Catalogue where
  toMemberValue = MemberValue_Artist
  fromMemberValue (MemberValue_Artist z) = Just z
  fromMemberValue _ = Nothing

instance DB.IsMemberValueOf Genre Catalogue where
  toMemberValue = MemberValue_Genre
  fromMemberValue (MemberValue_Genre z) = Just z
  fromMemberValue _ = Nothing

instance DB.IsMemberEdgeOf (DB.Edge () Artist) Catalogue where
  toMemberEdge = MemberEdge_UnitToArtist
  fromMemberEdge (MemberEdge_UnitToArtist z) = Just z
  fromMemberEdge _ = Nothing

instance DB.IsMemberEdgeOf (DB.Edge () Genre) Catalogue where
  toMemberEdge = MemberEdge_UnitToGenre
  fromMemberEdge (MemberEdge_UnitToGenre z) = Just z
  fromMemberEdge _ = Nothing

instance DB.IsMemberEdgeOf (DB.Edge Artist Genre) Catalogue where
  toMemberEdge = MemberEdge_ArtistToGenre
  fromMemberEdge (MemberEdge_ArtistToGenre z) = Just z
  fromMemberEdge _ = Nothing


data InsertArtist = InsertArtist Artist [Genre]

instance DB.Event InsertArtist Catalogue where
  type EventResult InsertArtist Catalogue = ()
  eventTransaction (InsertArtist artist genres) = 
    DB.Write $ insertArtist artist genres

instance DB.IsMemberEventOf InsertArtist Catalogue where
  toMemberEvent = MemberEvent_InsertArtist
  fromMemberEvent (MemberEvent_InsertArtist z) = Just z
  fromMemberEvent _ = Nothing

instance DB.IsMemberEventResultOf () Catalogue where
  toMemberEventResult = MemberEventResult_Unit
  fromMemberEventResult (MemberEventResult_Unit z) = Just z
  fromMemberEventResult _ = Nothing

data GetGenresByArtistName = GetGenresByArtistName Text

instance DB.Event GetGenresByArtistName Catalogue where
  type EventResult GetGenresByArtistName Catalogue = [Genre]
  eventTransaction (GetGenresByArtistName name) = 
    DB.Read $ getGenresByArtistName name

instance DB.IsMemberEventOf GetGenresByArtistName Catalogue where
  toMemberEvent = MemberEvent_GetGenresByArtistName
  fromMemberEvent (MemberEvent_GetGenresByArtistName z) = Just z
  fromMemberEvent _ = Nothing  

instance DB.IsMemberEventResultOf [Genre] Catalogue where
  toMemberEventResult = MemberEventResult_Genres
  fromMemberEventResult (MemberEventResult_Genres z) = Just z
  fromMemberEventResult _ = Nothing

-- 



deriving instance Eq Artist
deriving instance Eq Genre
deriving instance Eq (DB.Edge Artist Genre)
deriving instance Eq (DB.Edge () Artist)
deriving instance Eq (DB.Edge () Genre)
deriving instance Eq (DB.MemberEdge Catalogue)
deriving instance Eq (DB.MemberValue Catalogue)
deriving instance Generic Artist
deriving instance Generic Genre
deriving instance Generic (DB.Edge Artist Genre)
deriving instance Generic (DB.Edge () Artist)
deriving instance Generic (DB.Edge () Genre)
deriving instance Generic (DB.MemberEdge Catalogue)
deriving instance Generic (DB.MemberValue Catalogue)
deriving instance Generic (DB.MemberEvent Catalogue)
deriving instance Generic (DB.MemberEventResult Catalogue)
deriving instance Generic (InsertArtist)
deriving instance Generic (GetGenresByArtistName)
instance Hashable Artist
instance Hashable Genre
instance Hashable (DB.Edge Artist Genre)
instance Hashable (DB.Edge () Artist)
instance Hashable (DB.Edge () Genre)
instance Hashable (DB.MemberEdge Catalogue)
instance Hashable (DB.MemberValue Catalogue)

instance Serializable Artist IO
instance Serializable Genre IO
instance Serializable (DB.MemberValue Catalogue) IO
instance Serializable (DB.Edge Artist Genre) IO
instance Serializable (DB.Edge () Artist) IO
instance Serializable (DB.Edge () Genre) IO
instance Serializable (DB.MemberEdge Catalogue) IO
instance Serializable InsertArtist IO
instance Serializable GetGenresByArtistName IO
instance Serializable (DB.MemberEvent Catalogue) IO
instance Serializable (DB.MemberEventResult Catalogue) IO
