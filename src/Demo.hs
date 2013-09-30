
import TPM.GraphDB.Prelude
import qualified TPM.GraphDB as DB
import qualified TPM.GraphDB.Serialization as DB



data Catalogue deriving (Typeable, Generic)
data Artist = Artist {artistName :: Text} deriving (Eq, Show, Typeable, Generic)
data Genre = Genre {genreName :: Text} deriving (Eq, Show, Typeable, Generic)
data instance DB.Edge Catalogue () Artist = UnitToArtistByNameEdge Text | UnitToArtistEdge deriving (Eq, Show, Generic)
data instance DB.Edge Catalogue Artist Genre = ArtistToGenreEdge deriving (Eq, Show, Generic)
data instance DB.Edge Catalogue () Genre = UnitToGenreByGenreEdge Genre deriving (Eq, Show, Generic)

instance Hashable Artist
instance Hashable Genre
instance Hashable (DB.Edge Catalogue Artist Genre)
instance Hashable (DB.Edge Catalogue () Artist)
instance Hashable (DB.Edge Catalogue () Genre)



data instance DB.Term Catalogue =
  UnitTerm () | ArtistTerm Artist | GenreTerm Genre | 
  UnitToArtistEdgeTerm (DB.Edge Catalogue () Artist) |
  ArtistToGenreEdgeTerm (DB.Edge Catalogue Artist Genre)

instance DB.IsTerm Artist Catalogue where
  toTerm = ArtistTerm
  fromTerm (ArtistTerm z) = z

instance DB.IsTerm (DB.Edge Catalogue () Artist) Catalogue where
  toTerm = UnitToArtistEdgeTerm
  fromTerm (UnitToArtistEdgeTerm z) = z

instance DB.IsTerm (DB.Edge Catalogue Artist Genre) Catalogue where
  toTerm = ArtistToGenreEdgeTerm
  fromTerm (ArtistToGenreEdgeTerm z) = z

main = do
  db <- DB.new
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



