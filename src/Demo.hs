
import TPM.GraphDB.Prelude
import qualified TPM.GraphDB as DB



main = do
  db <- DB.new
  DB.run db $ InsertArtist (Artist "Metallica") [Genre "Metal", Genre "Rock"]
  DB.run db $ InsertArtist (Artist "Dire Straits") [Genre "Rock"]

  metallicaGenres <- DB.run db $ GetGenresByArtistName "Metallica"
  print metallicaGenres

insertArtist :: Artist -> [Genre] -> DB.Write Catalogue s ()
insertArtist artist genreList = do
  artistRef <- DB.newNode artist
  rootRef <- DB.getRoot
  DB.insertEdge UnitToArtistEdge artistRef rootRef
  DB.insertEdge (UnitToArtistByNameEdge (artistName artist)) artistRef rootRef
  -- Lookup existing genres and insert inexistent ones, then insert appropriate edges.
  for_ genreList $ \genre -> do
    genreRefs <- do
      existing <- DB.getTargets (UnitToGenreByGenreEdge genre) rootRef
      case existing of
        [] -> insertGenreAndGetRef genre >>= return . (:[])
        _ -> return existing
    for_ genreRefs $ \genreRef -> DB.insertEdge ArtistToGenreEdge genreRef artistRef

-- | 
-- Since this function returns a 'DB.NodeRef' it can't be directly used with 'DB.Event',
-- instead it can only be used in composition of others, like in 'insertArtist'.
insertGenreAndGetRef :: Genre -> DB.Write Catalogue s (DB.NodeRef Catalogue s Genre)
insertGenreAndGetRef genre = do
  new <- DB.newNode genre
  root <- DB.getRoot
  DB.insertEdge (UnitToGenreByGenreEdge genre) new root
  DB.insertEdge UnitToGenreEdge new root
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

data instance DB.UnionValue Catalogue =
  UnitUnionValue () | ArtistUnionValue Artist | GenreUnionValue Genre

data instance DB.UnionEdge Catalogue =
  UnitToArtistUnionEdge (DB.Edge () Artist) |
  UnitToGenreUnionEdge (DB.Edge () Genre) |
  ArtistToGenreUnionEdge (DB.Edge Artist Genre)

data instance DB.UnionEvent Catalogue = 
  InsertArtistUnionEvent InsertArtist |
  GetGenresByArtistNameUnionEvent GetGenresByArtistName

instance DB.IsUnionValueOf () Catalogue where
  toUnionValue = UnitUnionValue
  fromUnionValue (UnitUnionValue z) = Just z
  fromUnionValue _ = Nothing

instance DB.IsUnionValueOf Artist Catalogue where
  toUnionValue = ArtistUnionValue
  fromUnionValue (ArtistUnionValue z) = Just z
  fromUnionValue _ = Nothing

instance DB.IsUnionValueOf Genre Catalogue where
  toUnionValue = GenreUnionValue
  fromUnionValue (GenreUnionValue z) = Just z
  fromUnionValue _ = Nothing

instance DB.IsUnionEdgeOf (DB.Edge () Artist) Catalogue where
  toUnionEdge = UnitToArtistUnionEdge
  fromUnionEdge (UnitToArtistUnionEdge z) = Just z
  fromUnionEdge _ = Nothing

instance DB.IsUnionEdgeOf (DB.Edge () Genre) Catalogue where
  toUnionEdge = UnitToGenreUnionEdge
  fromUnionEdge (UnitToGenreUnionEdge z) = Just z
  fromUnionEdge _ = Nothing

instance DB.IsUnionEdgeOf (DB.Edge Artist Genre) Catalogue where
  toUnionEdge = ArtistToGenreUnionEdge
  fromUnionEdge (ArtistToGenreUnionEdge z) = Just z
  fromUnionEdge _ = Nothing



data InsertArtist = InsertArtist Artist [Genre]

instance DB.Event InsertArtist where
  type EventDB InsertArtist = Catalogue
  type EventTransaction InsertArtist = DB.Write
  type EventResult InsertArtist = ()
  transaction (InsertArtist artist genres) = insertArtist artist genres

instance DB.IsUnionEventOf InsertArtist Catalogue where
  toUnionEvent = InsertArtistUnionEvent
  fromUnionEvent (InsertArtistUnionEvent z) = Just z
  fromUnionEvent _ = Nothing

data GetGenresByArtistName = GetGenresByArtistName Text

instance DB.Event GetGenresByArtistName where
  type EventDB GetGenresByArtistName = Catalogue
  type EventTransaction GetGenresByArtistName = DB.Read
  type EventResult GetGenresByArtistName = [Genre]
  transaction (GetGenresByArtistName name) = getGenresByArtistName name

instance DB.IsUnionEventOf GetGenresByArtistName Catalogue where
  toUnionEvent = GetGenresByArtistNameUnionEvent
  fromUnionEvent (GetGenresByArtistNameUnionEvent z) = Just z
  fromUnionEvent _ = Nothing  



instance Eq (DB.Edge Artist Genre)
instance Eq (DB.Edge () Artist)
instance Eq (DB.UnionEdge Catalogue)
deriving instance Generic Artist
deriving instance Generic Genre
deriving instance Generic (DB.Edge Artist Genre)
deriving instance Generic (DB.Edge () Artist)
deriving instance Generic (DB.Edge () Genre)
deriving instance Generic (DB.UnionEdge Catalogue)
instance Hashable Artist
instance Hashable Genre
instance Hashable (DB.Edge Artist Genre)
instance Hashable (DB.Edge () Artist)
instance Hashable (DB.Edge () Genre)
instance Hashable (DB.UnionEdge Catalogue)



