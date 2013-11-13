
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
  -- O(1):
  new <- DB.newNode genre
  -- O(1):
  root <- DB.getRoot
  -- O(log n), where "n" is the number of edges from root node:
  DB.insertEdge root (UnitToGenreByGenreEdge genre) new
  -- O(log n), where "n" is the number of edges from root node:
  DB.insertEdge root UnitToGenreEdge new
  return new

getGenresByArtistName :: Text -> DB.Read Catalogue s [Genre]
getGenresByArtistName name = 
  -- O(1):
  DB.getRoot >>=
  -- O(log n), where "n" is the number of edges from root node:
  DB.getTargets (UnitToArtistByNameEdge name) >>=
  -- O(log n * m), where "n" is the number of edges from target node and 
  -- "m" is the number of target nodes:
  traverse (DB.getTargets ArtistToGenreEdge) >>=
  return . concat >>=
  -- O(n), where "n" is the number of result nodes, 
  -- i.e. the complexity of operation "getValue" is O(1):
  traverse DB.getValue



data Catalogue
data Artist = Artist {artistName :: Text} deriving (Show, Eq, Generic)
data Genre = Genre {genreName :: Text} deriving (Show, Eq, Generic)
data instance DB.Edge () Artist = UnitToArtistEdge | UnitToArtistByNameEdge Text deriving (Show, Eq, Generic)
data instance DB.Edge Artist Genre = ArtistToGenreEdge deriving (Show, Eq, Generic)
data instance DB.Edge () Genre = UnitToGenreEdge | UnitToGenreByGenreEdge Genre deriving (Show, Eq, Generic)

-- data instance DB.Edge Artist = ArtistOf | ArtistOfByName Text
-- data instance DB.Edge Genre = GenreOf | GenreOfByGenre Genre
-- OR
-- data instance DB.Edge Catalogue = Direct | ByName Text | ByGenre Genre
-- data instance DB.Node Catalogue = ArtistNode Artist | GenreNode Genre

DB.generateBoilerplate
  ''Catalogue
  [''Artist, ''Genre]


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

