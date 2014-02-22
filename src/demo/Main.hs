
import BasicPrelude
import GHC.Generics
import CerealPlus.Serializable
import qualified GraphDB.Graph as G
import qualified GraphDB.Persistence as GP
import qualified GraphDB.Transaction as G
import qualified GraphDB.Model as G
import qualified Data.Text as Text
import qualified Data.Time
import qualified System.Locale
import qualified GHC.DataSize


--------------
-- Data model.
--------------

data Catalogue = Catalogue Int deriving (Show, Eq, Generic)
data Release = Release Int Text ReleaseType Data.Time.Day deriving (Show, Eq, Generic)
data ReleaseType = StudioAlbum | Single deriving (Show, Eq, Generic)
data Track = Track Int deriving (Show, Eq, Generic)
data Recording = Recording Int Int RecordingType deriving (Show, Eq, Generic)
data RecordingType = StudioRecording | LiveRecording deriving (Show, Eq, Generic)
data Song = Song Text deriving (Show, Eq, Generic)
data TitleArtist = TitleArtist Bool deriving (Show, Eq, Generic)
data Artist = Artist Int Text deriving (Show, Eq, Generic)

--------------
-- Indexes and relations setup.
--------------

instance G.Edge Catalogue Artist where
  data Index Catalogue Artist =
    Index_Catalogue_Artist_UID Int |
    Index_Catalogue_Artist_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Artist uid name) = 
    map Index_Catalogue_Artist_SearchTerm (textToSearchTerms name) ++
    [Index_Catalogue_Artist_UID uid]

instance G.Edge Catalogue Release where
  data Index Catalogue Release =
    Index_Catalogue_Release_UID Int |
    Index_Catalogue_Release_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Release uid title typ date) =
    map Index_Catalogue_Release_SearchTerm (textToSearchTerms title) ++
    [Index_Catalogue_Release_UID uid]

instance G.Edge Catalogue Recording where
  data Index Catalogue Recording =
    Index_Catalogue_Recording_UID Int
    deriving (Show, Eq, Generic)
  indexes (Recording uid duration typ) =
    [Index_Catalogue_Recording_UID uid]

instance G.Edge Catalogue Song where
  data Index Catalogue Song =
    Index_Catalogue_Song_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Song title) =
    map Index_Catalogue_Song_SearchTerm (textToSearchTerms title)

instance G.Edge Artist Release where
  data Index Artist Release =
    -- We really don't need any constructors for this index, 
    -- since we won't be indexing anything.
    -- However, GHC can't generate deriving instances for constructorless types,
    -- so we create this dummy.
    -- 
    -- TODO (for devs): moving constraints from classes to functions might solve this.
    Index_Artist_Release
    deriving (Show, Eq, Generic)

instance G.Edge Artist Recording where
  data Index Artist Recording =
    Index_Artist_Recording
    deriving (Show, Eq, Generic)

instance G.Edge Artist Song where
  data Index Artist Song =
    Index_Artist_Song
    deriving (Show, Eq, Generic)

instance G.Edge Release Track where
  data Index Release Track =
    Index_Release_Track_Number Int
    deriving (Show, Eq, Generic)
  indexes (Track number) = [Index_Release_Track_Number number]

instance G.Edge Release TitleArtist where
  data Index Release TitleArtist =
    Index_Release_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Release_TitleArtist_Primary primary]

instance G.Edge Track Recording where
  data Index Track Recording =
    Index_Track_Recording
    deriving (Show, Eq, Generic)

instance G.Edge Recording Song where
  data Index Recording Song =
    Index_Recording_Song
    deriving (Show, Eq, Generic)

instance G.Edge Recording TitleArtist where
  data Index Recording TitleArtist =
    Index_Recording_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Recording_TitleArtist_Primary primary]

instance G.Edge Song Recording where
  data Index Song Recording =
    Index_Song_Recording
    deriving (Show, Eq, Generic)

instance G.Edge Song TitleArtist where
  data Index Song TitleArtist =
    Index_Song_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Song_TitleArtist_Primary primary]

instance G.Edge TitleArtist Artist where
  data Index TitleArtist Artist =
    Index_TitleArtist_Artist
    deriving (Show, Eq, Generic)

-- | A primitive search terms generator.
textToSearchTerms :: Text -> [Text]
textToSearchTerms = nub . map Text.toCaseFold . Text.words

-----------
-- Boilerplate.
-----------

-- Generate all boilerplate instances for nodes and edges, 
-- treating 'Catalogue' as a type of a root node's value. 
G.generateUnion ''Catalogue

-- A boilerplate for types not used as nodes.
instance Hashable ReleaseType
instance Hashable RecordingType
instance Serializable m ReleaseType
instance Serializable m RecordingType

-- Missing instances for 'Data.Time.Day'.
deriving instance Generic Data.Time.Day
instance Hashable Data.Time.Day

-----------
-- Transactions.
-----------

-- | Populate the DB with test data.
populate :: G.Backend b Catalogue => G.Write b Catalogue s ()
populate = do
  metallicaArtist <- do
    uid <- generateNewUID
    G.newNode $ Artist uid "Metallica"
  loadRelease <- do
    uid <- generateNewUID
    G.newNode $ Release uid "Load" StudioAlbum (mkDay "1996-07-01")
  untilItSleepsRelease <- do
    uid <- generateNewUID
    G.newNode $ Release uid "Until It Sleeps" Single (mkDay "1996-06-20")
  aintMyBitchSong <- G.newNode $ Song "Ain't My Bitch"
  untilItSleepsSong <- G.newNode $ Song "Until It Sleeps"
  
  root <- G.getRoot
  G.addTarget metallicaArtist root
  G.addTarget loadRelease root
  G.addTarget untilItSleepsRelease root
  
  addRecording loadRelease [metallicaArtist] aintMyBitchSong 1 (5*60+4) StudioRecording
  addRecording loadRelease [metallicaArtist] untilItSleepsSong 4 (4*60+28) StudioRecording
  addRecording untilItSleepsRelease [metallicaArtist] untilItSleepsSong 4 (4*60+36) StudioRecording
  
  addPrimaryArtist metallicaArtist loadRelease
  addPrimaryArtist metallicaArtist untilItSleepsRelease
  addPrimaryArtist metallicaArtist aintMyBitchSong
  
  where
    mkDay = Data.Time.readTime System.Locale.defaultTimeLocale "%Y-%m-%d"
    addRecording release primaryArtists song number duration typ = do
      track <- G.newNode $ Track number
      recording <- do
        uid <- generateNewUID
        G.newNode $ Recording uid duration typ
      forM_ primaryArtists $ \artist -> addPrimaryArtist artist recording
      G.addTarget recording track
      G.addTarget track release
      G.addTarget song recording
      G.addTarget recording song
    addPrimaryArtist artist node = do
      titleArtist <- G.newNode $ TitleArtist True
      G.addTarget titleArtist node
      G.addTarget artist titleArtist
      G.addTarget node artist
      return ()

-- | Use a counter stored in the root 'Catalogue' node to generate a new unique UID.
generateNewUID :: G.Backend b Catalogue => G.Write b Catalogue s Int
generateNewUID = do
  root <- G.getRoot
  Catalogue lastUID <- G.getValue root
  let newUID = lastUID + 1
  G.setValue root (Catalogue newUID)
  return newUID

-- | Search thru titles of songs, releases and artists.
search :: G.Backend b Catalogue => Text -> G.Read b Catalogue s [Either Artist (Either Release Song)]
search text = do
  artists <- searchByMkIndex terms Index_Catalogue_Artist_SearchTerm
  releases <- searchByMkIndex terms Index_Catalogue_Release_SearchTerm
  songs <- searchByMkIndex terms Index_Catalogue_Song_SearchTerm
  return $ map Left artists ++ map (Right . Left) releases ++ map (Right . Right) songs
  where
    terms = textToSearchTerms text
    searchByMkIndex terms mkIndex = do
      root <- G.getRoot
      groupedMatches <- forM terms $ \term ->
        G.getTargetsByIndex root (mkIndex term) >>=
        mapM G.getValue
      if null groupedMatches
        then return []
        else return $ foldr1 union groupedMatches

getRecordingsByArtistUID :: G.Backend b Catalogue => Int -> G.Read b Catalogue s [Recording]
getRecordingsByArtistUID uid =
  -- An example of a point-free query.
  G.getRoot >>=
  flip G.getTargetsByIndex (Index_Catalogue_Artist_UID uid) >>=
  mapM (flip G.getTargetsByType (undefined :: Recording)) >>=
  return . concat >>=
  mapM G.getValue

---------

main = do
  settings <- do
    paths <- GP.pathsFromDirectory "./dist/demo/db"
    let newGraph = G.new $ Catalogue 0
    return (100, paths, newGraph)
  GP.with settings $ \db -> do
    G.runWrite db populate
    putStrLn "Search for term 'It':"
    print =<< G.runRead db (search "It")
    putStrLn "Search for term 'metallica':"
    print =<< G.runRead db (search "metallica")
    putStrLn "Search for term 'load':"
    print =<< G.runRead db (search "load")
    putStrLn "All recordings of artists findable by term 'metallica':"
    -- Composing transactions:
    do
      results <- G.runRead db $
        search "metallica" >>=
        return . catMaybes . map (\case Left (Artist uid _) -> Just uid; _ -> Nothing) >>=
        mapM getRecordingsByArtistUID >>=
        return . concat
      print results
    putStrLn "Memory footprint (bytes):"
    print =<< GHC.DataSize.recursiveSize db
    putStrLn "Total amounts of nodes and edges in the graph:"
    print =<< G.runRead db G.getStats




