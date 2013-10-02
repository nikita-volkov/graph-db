
import TPM.GraphDB.Prelude
import qualified TPM.GraphDB as DB



data Catalogue deriving (Typeable, Generic)
data Artist = Artist {artistName :: Text} deriving (Eq, Show, Typeable, Generic)
data Genre = Genre {genreName :: Text} deriving (Eq, Show, Typeable, Generic)
data instance DB.Edge () Artist = UnitToArtistByNameEdge Text | UnitToArtistEdge deriving (Eq, Show, Generic)
data instance DB.Edge Artist Genre = ArtistToGenreEdge deriving (Eq, Show, Generic)
data instance DB.Edge () Genre = UnitToGenreByNameEdge Text deriving (Eq, Show, Generic)

instance Hashable Artist
instance Hashable Genre
instance Hashable (DB.Edge Artist Genre)
instance Hashable (DB.Edge () Artist)
instance Hashable (DB.Edge () Genre)

data instance DB.UnionValue Catalogue =
  UnitUV () | ArtistUV Artist | GenreUV Genre

data instance DB.UnionEdge Catalogue =
  UnitToArtistUE (DB.Edge () Artist) |
  ArtistToGenreUE (DB.Edge Artist Genre)
  deriving (Generic, Eq)

instance Hashable (DB.UnionEdge Catalogue)

instance DB.IsUnionValueOf () Catalogue where
  toUnionValue = UnitUV
  fromUnionValue (UnitUV z) = Just z
  fromUnionValue _ = Nothing

instance DB.IsUnionValueOf Artist Catalogue where
  toUnionValue = ArtistUV
  fromUnionValue (ArtistUV z) = Just z
  fromUnionValue _ = Nothing

instance DB.IsUnionValueOf Genre Catalogue where
  toUnionValue = GenreUV
  fromUnionValue (GenreUV z) = Just z
  fromUnionValue _ = Nothing

instance DB.IsUnionEdgeOf (DB.Edge () Artist) Catalogue where
  toUnionEdge = UnitToArtistUE
  fromUnionEdge (UnitToArtistUE z) = Just z
  fromUnionEdge _ = Nothing

instance DB.IsUnionEdgeOf (DB.Edge Artist Genre) Catalogue where
  toUnionEdge = ArtistToGenreUE
  fromUnionEdge (ArtistToGenreUE z) = Just z
  fromUnionEdge _ = Nothing



main = do
  db :: DB.GraphDB Catalogue <- DB.new
  -- DB.run db $ InsertArtist (Artist "A")

  undefined

insertArtist :: Artist -> DB.Write Catalogue s ()
insertArtist artist = do
  new <- DB.newNode artist
  root <- DB.getRoot
  DB.insertEdge UnitToArtistEdge root new
  DB.insertEdge (UnitToArtistByNameEdge (artistName artist)) root new

getGenresByArtistName :: Text -> DB.Read Catalogue s [Genre]
getGenresByArtistName name = 
  DB.getRoot >>=
  DB.getTargets (UnitToArtistByNameEdge name) >>=
  traverse (DB.getTargets ArtistToGenreEdge) >>=
  return . concat >>=
  traverse DB.getValue


  
-- data instance DB.EventUnion Catalogue = 
--   InsertArtistUE InsertArtist

-- data InsertArtist = InsertArtist Artist

-- instance DB.Event InsertArtist where
--   type EventTag = Catalogue
--   type EventTransaction = DB.Write
--   type EventResult = ()
--   transaction (InsertArtist artist) = insertArtist artist



