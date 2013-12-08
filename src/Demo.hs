
import GraphDB.Prelude
import qualified GraphDB as G
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

instance G.Edge Catalogue Catalogue Artist where
  data Index Catalogue Catalogue Artist =
    Index_Catalogue_Artist_UID Int |
    Index_Catalogue_Artist_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Artist uid name) = 
    map Index_Catalogue_Artist_SearchTerm (textToSearchTerms name) ++
    [Index_Catalogue_Artist_UID uid]

instance G.Edge Catalogue Catalogue Release where
  data Index Catalogue Catalogue Release =
    Index_Catalogue_Release_UID Int |
    Index_Catalogue_Release_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Release uid title typ date) =
    map Index_Catalogue_Release_SearchTerm (textToSearchTerms title) ++
    [Index_Catalogue_Release_UID uid]

instance G.Edge Catalogue Catalogue Recording where
  data Index Catalogue Catalogue Recording =
    Index_Catalogue_Recording_UID Int
    deriving (Show, Eq, Generic)
  indexes (Recording uid duration typ) =
    [Index_Catalogue_Recording_UID uid]

instance G.Edge Catalogue Catalogue Song where
  data Index Catalogue Catalogue Song =
    Index_Catalogue_Song_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Song title) =
    map Index_Catalogue_Song_SearchTerm (textToSearchTerms title)

instance G.Edge Catalogue Artist Release where
  data Index Catalogue Artist Release =
    -- We really don't need any constructors for this index, 
    -- since we won't be indexing anything.
    -- However, GHC can't generate deriving instances for constructorless types,
    -- so we create this dummy.
    -- 
    -- TODO (for devs): moving constraints from classes to functions might solve this.
    Index_Artist_Release
    deriving (Show, Eq, Generic)

instance G.Edge Catalogue Artist Recording where
  data Index Catalogue Artist Recording =
    Index_Artist_Recording
    deriving (Show, Eq, Generic)

instance G.Edge Catalogue Artist Song where
  data Index Catalogue Artist Song =
    Index_Artist_Song
    deriving (Show, Eq, Generic)

instance G.Edge Catalogue Release Track where
  data Index Catalogue Release Track =
    Index_Release_Track_Number Int
    deriving (Show, Eq, Generic)
  indexes (Track number) = [Index_Release_Track_Number number]

instance G.Edge Catalogue Release TitleArtist where
  data Index Catalogue Release TitleArtist =
    Index_Release_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Release_TitleArtist_Primary primary]

instance G.Edge Catalogue Track Recording where
  data Index Catalogue Track Recording =
    Index_Track_Recording
    deriving (Show, Eq, Generic)

instance G.Edge Catalogue Recording Song where
  data Index Catalogue Recording Song =
    Index_Recording_Song
    deriving (Show, Eq, Generic)

instance G.Edge Catalogue Recording TitleArtist where
  data Index Catalogue Recording TitleArtist =
    Index_Recording_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Recording_TitleArtist_Primary primary]

instance G.Edge Catalogue Song Recording where
  data Index Catalogue Song Recording =
    Index_Song_Recording
    deriving (Show, Eq, Generic)

instance G.Edge Catalogue Song TitleArtist where
  data Index Catalogue Song TitleArtist =
    Index_Song_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Song_TitleArtist_Primary primary]

instance G.Edge Catalogue TitleArtist Artist where
  data Index Catalogue TitleArtist Artist =
    Index_TitleArtist_Artist
    deriving (Show, Eq, Generic)

-- | A primitive search terms generator.
textToSearchTerms :: Text -> [Text]
textToSearchTerms = nub . map Text.toCaseFold . Text.words

-----------
-- Events.
-----------

-- | Populate the DB with test data.
populate :: G.Write Catalogue s ()
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
generateNewUID :: G.Write Catalogue s Int
generateNewUID = do
  root <- G.getRoot
  Catalogue lastUID <- G.getValue root
  let newUID = lastUID + 1
  G.setValue (Catalogue newUID) root
  return newUID

-- | Search thru titles of songs, releases and artists.
search :: Text -> G.Read Catalogue s [Either Artist (Either Release Song)]
search text = do
  artists <- searchByMkIndex Index_Catalogue_Artist_SearchTerm
  releases <- searchByMkIndex Index_Catalogue_Release_SearchTerm
  songs <- searchByMkIndex Index_Catalogue_Song_SearchTerm
  return $ map Left artists ++ map (Right . Left) releases ++ map (Right . Right) songs
  where
    terms = textToSearchTerms text
    searchByMkIndex :: 
      (G.Edge Catalogue Catalogue b, Eq b) => 
      (Text -> G.Index Catalogue Catalogue b) -> G.Read Catalogue s [b]
    searchByMkIndex mkIndex = do
      root <- G.getRoot
      groupedMatches <- forM terms $ \term ->
        G.getTargetsByIndex (mkIndex term) root >>=
        mapM G.getValue
      if null groupedMatches
        then return []
        else return $ foldr1 union groupedMatches

getRecordingsByArtistUID :: Int -> G.Read Catalogue s [Recording]
getRecordingsByArtistUID uid =
  -- An example of point-free query.
  G.getRoot >>=
  G.getTargetsByIndex (Index_Catalogue_Artist_UID uid) >>=
  mapM (G.getTargetsByType (undefined :: Recording)) >>=
  return . concat >>=
  mapM G.getValue

getStats :: G.Read Catalogue s (Int, Int)
getStats = G.getStats

-----------
-- Boilerplate.
-----------

G.generateBoilerplate ''Catalogue

-- A boilerplate for types not used as nodes. Boilerplate generator currently skips those.
instance Hashable ReleaseType
instance Hashable RecordingType
instance Serializable m ReleaseType
instance Serializable m RecordingType

-- Missing instances for 'Data.Time.Day'.
deriving instance Generic Data.Time.Day
instance Hashable Data.Time.Day

-----------

-- | Get a prepopulated db for testing.
initializeDB :: IO (G.Engine Catalogue)
initializeDB = do
  db <- G.startEngine initRoot =<< getLocalUnpersistedMode
  G.runEvent db Populate
  return db
  where
    initRoot = Catalogue 0
    getLocalPersistedMode = return . G.Mode_Local . Just . (100,) =<< G.pathsFromDirectory dir
      where
        dir = "./dist/demo/"
    getLocalUnpersistedMode = return . G.Mode_Local $ Nothing

main = do
  db <- initializeDB
  putStrLn "Search for term 'It':"
  print =<< G.runEvent db (Search "It")
  putStrLn "Search for term 'metallica':"
  print =<< G.runEvent db (Search "metallica")
  putStrLn "Search for term 'load':"
  print =<< G.runEvent db (Search "load")
  putStrLn "All recordings of artists findable by term 'metallica':"
  -- Following is an example of compostion of events.
  -- Though it's recommended to perform all the composition inside the transactions
  -- to produce most specific final events.
  -- This way you'll be guaranteed that no concurrent changes to DB happen 
  -- in-between the composed transactions.
  print
    =<< return . concat
    =<< mapM (G.runEvent db . GetRecordingsByArtistUID)
    =<< return . catMaybes . map (\case Left (Artist uid _) -> Just uid; _ -> Nothing)
    =<< G.runEvent db (Search "metallica")

  putStrLn "Memory footprint (bytes):"
  print =<< GHC.DataSize.recursiveSize db
  putStrLn "Total amounts of nodes and edges in the graph:"
  print =<< G.runEvent db GetStats

  G.shutdownEngine db




