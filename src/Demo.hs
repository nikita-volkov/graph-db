
import TPM.GraphDB.Prelude
import qualified TPM.GraphDB as DB



main = do
  db <- DB.new
  DB.run db $ InsertArtist (Artist "A")

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

data Catalogue
data Artist = Artist {artistName :: Text}
data Genre = Genre {genreName :: Text}
data instance DB.Edge () Artist = UnitToArtistByNameEdge Text | UnitToArtistEdge
data instance DB.Edge Artist Genre = ArtistToGenreEdge
data instance DB.Edge () Genre = UnitToGenreByNameEdge Text



--------------------------
-- The Boilerplate
--------------------------

data instance DB.UnionValue Catalogue =
  UnitUnionValue () | ArtistUnionValue Artist | GenreUnionValue Genre

data instance DB.UnionEdge Catalogue =
  UnitToArtistUnionEdge (DB.Edge () Artist) |
  ArtistToGenreUnionEdge (DB.Edge Artist Genre)

data instance DB.UnionEvent Catalogue = 
  InsertArtistUnionEvent InsertArtist 

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

instance DB.IsUnionEdgeOf (DB.Edge Artist Genre) Catalogue where
  toUnionEdge = ArtistToGenreUnionEdge
  fromUnionEdge (ArtistToGenreUnionEdge z) = Just z
  fromUnionEdge _ = Nothing



data InsertArtist = InsertArtist Artist

instance DB.Event InsertArtist where
  type EventDB InsertArtist = Catalogue
  type EventTransaction InsertArtist = DB.Write
  type EventResult InsertArtist = ()
  transaction (InsertArtist artist) = insertArtist artist

instance DB.IsUnionEventOf InsertArtist Catalogue where
  toUnionEvent = InsertArtistUnionEvent
  fromUnionEvent (InsertArtistUnionEvent z) = Just z
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



