
import TPM.Prelude
import qualified TPM.GraphDB as DB



data Artist = Artist {artistName :: Text}
data Genre = Genre {genreName :: Text}
data instance DB.Edge () Artist = UnitToArtistByNameEdge Text | UnitToArtistEdge
data instance DB.Edge Artist Genre = ArtistToGenreEdge
data instance DB.Edge () Genre = UnitToGenreByGenreEdge Genre



main = do
  db <- DB.new
  -- DB.run db $ InsertArtist (Artist "A")

  undefined

insertArtist :: Artist -> DB.Write s ()
insertArtist artist = do
  new <- DB.newNode artist
  root <- DB.getRoot
  DB.insertEdge UnitToArtistEdge root new
  DB.insertEdge (UnitToArtistByNameEdge (artistName artist)) root new

getGenresByArtistName :: Text -> DB.Read s [Genre]
getGenresByArtistName name = 
  DB.getRoot >>=
  DB.getTargets (UnitToArtistByNameEdge name) >>=
  traverse (DB.getTargets ArtistToGenreEdge) >>=
  return . concat >>=
  traverse DB.getValue



