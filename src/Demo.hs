
import TPM.GraphDB.Prelude
import qualified TPM.GraphDB as DB



data Catalogue deriving (Typeable, Generic)
data Artist = Artist {artistName :: Text} deriving (Eq, Show, Typeable, Generic)
data Genre = Genre {genreName :: Text} deriving (Eq, Show, Typeable, Generic)
data instance DB.Edge () Artist = UnitToArtistByNameEdge Text | UnitToArtistEdge deriving (Eq, Show, Generic)
data instance DB.Edge Artist Genre = ArtistToGenreEdge deriving (Eq, Show, Generic)
data instance DB.Edge () Genre = UnitToGenreByGenreEdge Genre deriving (Eq, Show, Generic)

instance Hashable Artist
instance Hashable Genre
instance Hashable (DB.Edge Artist Genre)
instance Hashable (DB.Edge () Artist)
instance Hashable (DB.Edge () Genre)

data instance DB.ValueUnion Catalogue =
  UnitTerm () | ArtistTerm Artist | GenreTerm Genre

data instance DB.EdgeUnion Catalogue =
  UnitToArtistEdgeTerm (DB.Edge () Artist) |
  ArtistToGenreEdgeTerm (DB.Edge Artist Genre)
  deriving (Generic, Eq)

instance Hashable (DB.EdgeUnion Catalogue)

instance DB.IsValue () Catalogue where
  toValueUnion = UnitTerm
  fromValueUnion (UnitTerm z) = z

instance DB.IsValue Artist Catalogue where
  toValueUnion = ArtistTerm
  fromValueUnion (ArtistTerm z) = z

instance DB.IsValue Genre Catalogue where
  toValueUnion = GenreTerm
  fromValueUnion (GenreTerm z) = z

instance DB.IsEdge (DB.Edge () Artist) Catalogue where
  toEdgeUnion = UnitToArtistEdgeTerm
  fromEdgeUnion (UnitToArtistEdgeTerm z) = z

instance DB.IsEdge (DB.Edge Artist Genre) Catalogue where
  toEdgeUnion = ArtistToGenreEdgeTerm
  fromEdgeUnion (ArtistToGenreEdgeTerm z) = z

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



