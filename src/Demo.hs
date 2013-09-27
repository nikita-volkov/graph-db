
import TPM.Prelude
import qualified TPM.GraphDB as DB



data Catalogue
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



