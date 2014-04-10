module APITests.Catalogue where


import APITests.Prelude
import qualified GraphDB as G
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Time
import qualified System.Locale
import qualified GHC.DataSize
import qualified System.IO.Unsafe


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
textToSearchTerms = nub . map Data.Text.toCaseFold . Data.Text.words


-----------
-- Boilerplate.
-----------

-- Generate all boilerplate instances for nodes and edges, 
-- treating 'Catalogue' as a type of a root node's value. 
G.deriveUnion ''Catalogue

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
populate :: G.Write s Catalogue t ()
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
  G.addTarget root metallicaArtist
  G.addTarget root loadRelease
  G.addTarget root untilItSleepsRelease
  
  addRecording loadRelease [metallicaArtist] aintMyBitchSong 1 (5*60+4) StudioRecording
  addRecording loadRelease [metallicaArtist] untilItSleepsSong 4 (4*60+28) StudioRecording
  addRecording untilItSleepsRelease [metallicaArtist] untilItSleepsSong 4 (4*60+36) StudioRecording
  
  addPrimaryArtist loadRelease metallicaArtist
  addPrimaryArtist untilItSleepsRelease metallicaArtist
  addPrimaryArtist aintMyBitchSong metallicaArtist
  
  where
    mkDay = Data.Time.readTime System.Locale.defaultTimeLocale "%Y-%m-%d"
    addRecording release primaryArtists song number duration typ = do
      track <- G.newNode $ Track number
      recording <- do
        uid <- generateNewUID
        G.newNode $ Recording uid duration typ
      forM_ primaryArtists $ \artist -> addPrimaryArtist recording artist
      G.addTarget track recording
      G.addTarget release track
      G.addTarget recording song
      G.addTarget song recording
    addPrimaryArtist node artist = do
      titleArtist <- G.newNode $ TitleArtist True
      G.addTarget node titleArtist
      G.addTarget titleArtist artist
      G.addTarget artist node
      return ()

-- | Use a counter stored in the root 'Catalogue' node to generate a new unique UID.
generateNewUID :: G.Write s Catalogue t Int
generateNewUID = do
  root <- G.getRoot
  Catalogue lastUID <- G.getValue root
  let newUID = lastUID + 1
  G.setValue root (Catalogue newUID)
  return newUID

-- | Search thru titles of songs, releases and artists.
search :: Text -> G.ReadOrWrite s Catalogue t [Either Artist (Either Release Song)]
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

getRecordingsByArtistUID :: Int -> G.ReadOrWrite s Catalogue t [Recording]
getRecordingsByArtistUID uid =
  -- An example of a point-free query.
  G.getRoot >>=
  flip G.getTargetsByIndex (Index_Catalogue_Artist_UID uid) >>=
  mapM (flip G.getTargetsByType (undefined :: Recording)) >>=
  return . concat >>=
  mapM G.getValue

measureMemoryFootprint :: G.ReadOrWrite s Catalogue t Int
measureMemoryFootprint =
  G.getRoot >>= return . System.IO.Unsafe.unsafePerformIO . GHC.DataSize.recursiveSize

