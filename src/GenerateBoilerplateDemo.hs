
import TPM.GraphDB.Prelude
import qualified TPM.GraphDB as DB
import qualified Data.SafeCopy as SafeCopy



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


DB.generateBoilerplate ''Catalogue


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

