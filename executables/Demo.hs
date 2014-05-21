
import BasicPrelude
import GHC.Generics (Generic)
import qualified GraphDB as G
import qualified Data.Text as Text


-- * Model
-------------------------

data Catalogue = Catalogue deriving (Show, Eq, Generic)
data Artist = Artist Name deriving (Show, Eq, Generic)
data Genre = Genre Name deriving (Show, Eq, Generic)
type Name = Text

-- * Relations
-------------------------

instance G.Edge Catalogue Artist where
  data Index Catalogue Artist =
    Catalogue_Artist |
    Catalogue_Artist_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Artist name) = 
    Catalogue_Artist : 
    searchTerms
    where
      searchTerms = map Catalogue_Artist_SearchTerm $ Text.words name

instance G.Edge Catalogue Genre where
  data Index Catalogue Genre =
    Catalogue_Genre |
    Catalogue_Genre_Name Text
    deriving (Show, Eq, Generic)
  indexes (Genre name) =
    Catalogue_Genre : 
    Catalogue_Genre_Name name :
    []

instance G.Edge Genre Artist where
  data Index Genre Artist =
    Genre_Artist
    deriving (Show, Eq, Generic)
  indexes (Artist name) =
    [Genre_Artist]

G.deriveSetup ''Catalogue



main = do
  putStrLn "Restoring the graph from the storage."
  G.runPersistentSession (Catalogue, "./dist/demo/db", 1) $ do
    do
      G.read G.getStats >>= \case
        (1, 0, 0) -> do
          liftIO $ putStrLn "Graph is empty. Populating."
          G.write $ populate
        _ -> return ()
    do
      G.read G.getStats >>= \(nodes, edges, indexes) ->
        liftIO $ putStrLn $ 
          "There's " <> show nodes <> " nodes, " <> show edges <> " edges " <>
          "and " <> show indexes <> " indexes in the graph."
    do
      liftIO $ putStrLn "Artists by the search term \"The\":"
      liftIO . print =<< do 
        G.read $
          G.getRoot >>= 
          flip G.getTargets (Catalogue_Artist_SearchTerm "The") >>=
          mapM G.getValue
    do
      liftIO $ putStrLn "Artists by the genre named \"Rock\":"
      liftIO . print =<< do 
        G.read $
          G.getRoot >>=
          flip G.getTargets (Catalogue_Genre_Name "Rock") >>=
          fmap join . mapM (flip G.getTargets (Genre_Artist)) >>=
          mapM G.getValue

populate :: G.Write s Catalogue t ()
populate = do
  root <- G.getRoot

  rollingStones <- G.newNode $ Artist "The Rolling Stones"
  beatles <- G.newNode $ Artist "The Beatles"
  metallica <- G.newNode $ Artist "Metallica"
  nirvana <- G.newNode $ Artist "Nirvana"

  rock <- G.newNode $ Genre "Rock"
  grunge <- G.newNode $ Genre "Grunge"
  metal <- G.newNode $ Genre "Metal"

  G.addTarget root rollingStones
  G.addTarget root beatles
  G.addTarget root metallica
  G.addTarget root nirvana

  G.addTarget root rock
  G.addTarget root grunge
  G.addTarget root metal

  G.addTarget rock rollingStones
  G.addTarget rock beatles
  G.addTarget rock metallica
  G.addTarget rock nirvana

  G.addTarget grunge nirvana

  G.addTarget metal metallica


