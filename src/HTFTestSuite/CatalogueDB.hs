module HTFTestSuite.CatalogueDB where


import GraphDB.Prelude
import qualified GraphDB as G
import qualified Data.Text as Text
import qualified Data.Time
import qualified System.Locale
import qualified Test.QuickCheck as QC hiding (oneof, listOf, elements)
import qualified QuickCheck.GenT as QC
import Test.QuickCheck.Instances ()



-----------

type CatalogueDB = G.Engine Catalogue

--------------
-- Data model.
--------------

data Catalogue = Catalogue Int deriving (Show, Eq, Generic)
data Release = Release Int Text ReleaseType Data.Time.Day deriving (Show, Eq, Generic)
data ReleaseType = StudioAlbum | Single deriving (Show, Eq, Generic, Enum, Bounded)
data Track = Track Int deriving (Show, Eq, Generic)
data Recording = Recording Int Int RecordingType deriving (Show, Eq, Generic)
data RecordingType = StudioRecording | LiveRecording deriving (Show, Eq, Generic, Enum, Bounded)
-- TODO: just add UID to song
data Song = Song Text deriving (Show, Eq, Generic)
data TitleArtist = TitleArtist Bool deriving (Show, Eq, Generic)
data Artist = Artist Int Text deriving (Show, Eq, Generic)

--------------
-- Indexes and relations setup.
--------------

instance G.Reachable Catalogue Catalogue Artist where
  data Index Catalogue Catalogue Artist =
    Index_Catalogue_Artist_UID Int |
    Index_Catalogue_Artist_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Artist uid name) = 
    map Index_Catalogue_Artist_SearchTerm (textToSearchTerms name) ++
    [Index_Catalogue_Artist_UID uid]

instance G.Reachable Catalogue Catalogue Release where
  data Index Catalogue Catalogue Release =
    Index_Catalogue_Release_UID Int |
    Index_Catalogue_Release_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Release uid title typ date) =
    map Index_Catalogue_Release_SearchTerm (textToSearchTerms title) ++
    [Index_Catalogue_Release_UID uid]

instance G.Reachable Catalogue Catalogue Recording where
  data Index Catalogue Catalogue Recording =
    Index_Catalogue_Recording_UID Int
    deriving (Show, Eq, Generic)
  indexes (Recording uid duration typ) =
    [Index_Catalogue_Recording_UID uid]

instance G.Reachable Catalogue Catalogue Song where
  data Index Catalogue Catalogue Song =
    Index_Catalogue_Song_SearchTerm Text
    deriving (Show, Eq, Generic)
  indexes (Song title) =
    map Index_Catalogue_Song_SearchTerm (textToSearchTerms title)

instance G.Reachable Catalogue Artist Release where
  data Index Catalogue Artist Release =
    -- We really don't need any constructors for this index, 
    -- since we won't be indexing anything.
    -- However, GHC can't generate deriving instances for constructorless types,
    -- so we create this dummy.
    -- 
    -- TODO (for devs): moving constraints from classes to functions might solve this.
    Index_Artist_Release
    deriving (Show, Eq, Generic)

instance G.Reachable Catalogue Artist Recording where
  data Index Catalogue Artist Recording =
    Index_Artist_Recording
    deriving (Show, Eq, Generic)

instance G.Reachable Catalogue Artist Song where
  data Index Catalogue Artist Song =
    Index_Artist_Song
    deriving (Show, Eq, Generic)

instance G.Reachable Catalogue Release Track where
  data Index Catalogue Release Track =
    Index_Release_Track_Number Int
    deriving (Show, Eq, Generic)
  indexes (Track number) = [Index_Release_Track_Number number]

instance G.Reachable Catalogue Release TitleArtist where
  data Index Catalogue Release TitleArtist =
    Index_Release_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Release_TitleArtist_Primary primary]

instance G.Reachable Catalogue Track Recording where
  data Index Catalogue Track Recording =
    Index_Track_Recording
    deriving (Show, Eq, Generic)

instance G.Reachable Catalogue Recording Song where
  data Index Catalogue Recording Song =
    Index_Recording_Song
    deriving (Show, Eq, Generic)

instance G.Reachable Catalogue Recording TitleArtist where
  data Index Catalogue Recording TitleArtist =
    Index_Recording_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Recording_TitleArtist_Primary primary]

instance G.Reachable Catalogue Song Recording where
  data Index Catalogue Song Recording =
    Index_Song_Recording
    deriving (Show, Eq, Generic)

instance G.Reachable Catalogue Song TitleArtist where
  data Index Catalogue Song TitleArtist =
    Index_Song_TitleArtist_Primary Bool
    deriving (Show, Eq, Generic)
  indexes (TitleArtist primary) = [Index_Song_TitleArtist_Primary primary]

instance G.Reachable Catalogue TitleArtist Artist where
  data Index Catalogue TitleArtist Artist =
    Index_TitleArtist_Artist
    deriving (Show, Eq, Generic)

-- | A primitive search terms generator.
textToSearchTerms :: Text -> [Text]
textToSearchTerms = nub . map Text.toCaseFold . Text.words

-----------
-- Foreign boilerplate.
-----------

-- A boilerplate for types not used as nodes. Boilerplate generator currently skips those.
instance Hashable ReleaseType
instance Hashable RecordingType
instance Serializable m ReleaseType
instance Serializable m RecordingType

-- Missing instances for 'Data.Time.Day'.
deriving instance Generic Data.Time.Day
instance Hashable Data.Time.Day

-----------
-- Events.
-----------

getArtistByUID :: Int -> G.Read Catalogue s (Maybe Artist)
getArtistByUID uid =
  G.getRoot >>= 
  G.getTargetsByIndex (Index_Catalogue_Artist_UID uid) >>= 
  return . headZ >>=
  mapM G.getValue

setArtistByUID :: Artist -> Int -> G.Write Catalogue s ()
setArtistByUID newValue uid = do
  artists <- G.getTargetsByIndex (Index_Catalogue_Artist_UID uid) =<< G.getRoot
  forM_ artists $ G.setValue newValue

generateNewUID :: G.Write Catalogue s Int
generateNewUID = do
  root <- G.getRoot
  Catalogue lastUID <- G.getValue root
  let newUID = lastUID + 1
  G.setValue (Catalogue newUID) root
  return newUID

linkArtistToRelease :: Artist -> Release -> G.Write Catalogue s ()
linkArtistToRelease (Artist artistUID _) (Release releaseUID _ _ _) = do
  root <- G.getRoot
  source <- head <$> G.getTargetsByIndex (Index_Catalogue_Artist_UID artistUID) root
  target <- head <$> G.getTargetsByIndex (Index_Catalogue_Release_UID releaseUID) root
  G.addTarget target source

linkArtistToRecording :: Artist -> Recording -> G.Write Catalogue s ()
linkArtistToRecording (Artist artistUID _) (Recording recordingUID _ _) = do
  root <- G.getRoot
  source <- head <$> G.getTargetsByIndex (Index_Catalogue_Artist_UID artistUID) root
  target <- head <$> G.getTargetsByIndex (Index_Catalogue_Recording_UID recordingUID) root
  G.addTarget target source

linkReleaseToArtist :: Release -> TitleArtist -> Artist -> G.Write Catalogue s ()
linkReleaseToArtist (Release releaseUID _ _ _) titleArtist (Artist artistUID _) = do
  root <- G.getRoot
  releaseNode <- head <$> G.getTargetsByIndex (Index_Catalogue_Release_UID releaseUID) root
  artistNode <- head <$> G.getTargetsByIndex (Index_Catalogue_Artist_UID artistUID) root
  titleArtistNode <- G.newNode titleArtist
  G.addTarget titleArtistNode releaseNode
  G.addTarget artistNode titleArtistNode

linkReleaseToRecording :: Release -> Track -> Recording -> G.Write Catalogue s ()
linkReleaseToRecording (Release releaseUID _ _ _) track (Recording recordingUID _ _) = do
  root <- G.getRoot
  releaseNode <- head <$> G.getTargetsByIndex (Index_Catalogue_Release_UID releaseUID) root
  recordingNode <- head <$> G.getTargetsByIndex (Index_Catalogue_Recording_UID recordingUID) root
  trackNode <- G.newNode track
  G.addTarget trackNode releaseNode
  G.addTarget recordingNode trackNode

linkRecordingToArtist :: Recording -> TitleArtist -> Artist -> G.Write Catalogue s ()
linkRecordingToArtist (Recording recordingUID _ _) titleArtist (Artist artistUID _) = do
  root <- G.getRoot
  recordingNode <- head <$> G.getTargetsByIndex (Index_Catalogue_Recording_UID recordingUID) root
  artistNode <- head <$> G.getTargetsByIndex (Index_Catalogue_Artist_UID artistUID) root
  titleArtistNode <- G.newNode titleArtist
  G.addTarget titleArtistNode recordingNode
  G.addTarget artistNode titleArtistNode

linkRecordingToSong :: Recording -> Song -> G.Write Catalogue s ()
linkRecordingToSong = undefined

linkSongToArtist :: Song -> TitleArtist -> Artist -> G.Write Catalogue s ()
linkSongToArtist = undefined

linkSongToRecording :: Song -> Recording -> G.Write Catalogue s ()
linkSongToRecording = undefined

linkCatalogueToArtist :: Artist -> G.Write Catalogue s ()
linkCatalogueToArtist artist = do
  root <- G.getRoot
  artistNode <- G.newNode artist
  G.addTarget artistNode root

linkCatalogueToRelease :: Release -> G.Write Catalogue s ()
linkCatalogueToRelease release = do
  root <- G.getRoot
  releaseNode <- G.newNode release
  G.addTarget releaseNode root

linkCatalogueToRecording :: Recording -> G.Write Catalogue s ()
linkCatalogueToRecording recording = do
  root <- G.getRoot
  recordingNode <- G.newNode recording
  G.addTarget recordingNode root

getAllReleases :: G.Read Catalogue s [Release]
getAllReleases = G.getRoot >>= G.getTargetsByType (undefined :: Release) >>= mapM G.getValue

getAllRecordings :: G.Read Catalogue s [Recording]
getAllRecordings = G.getRoot >>= G.getTargetsByType (undefined :: Recording) >>= mapM G.getValue

getAllSongs :: G.Read Catalogue s [Song]
getAllSongs = G.getRoot >>= G.getTargetsByType (undefined :: Song) >>= mapM G.getValue

getAllArtists :: G.Read Catalogue s [Artist]
getAllArtists = G.getRoot >>= G.getTargetsByType (undefined :: Artist) >>= mapM G.getValue

removeArtistByUID :: Int -> G.Write Catalogue s ()
removeArtistByUID uid = do
  root <- G.getRoot
  artist <- head <$> G.getTargetsByIndex (Index_Catalogue_Artist_UID uid) root
  G.removeTarget artist root
  allTitleArtists <- do
    recordingTitleArtists <- 
      G.getTargetsByType (undefined :: Recording) root >>=
      fmap concat . mapM (G.getTargetsByType (undefined :: TitleArtist))
    releaseTitleArtists <-
      G.getTargetsByType (undefined :: Release) root >>=
      fmap concat . mapM (G.getTargetsByType (undefined :: TitleArtist))
    return $ nub $ recordingTitleArtists ++ releaseTitleArtists
  forM_ allTitleArtists $ \titleArtist ->
    G.removeTarget artist titleArtist

getArtistsByReleaseUID :: Int -> G.Read Catalogue s [Artist]
getArtistsByReleaseUID uid =
  G.getRoot >>=
  G.getTargetsByIndex (Index_Catalogue_Release_UID uid) >>=
  fmap join . mapM (G.getTargetsByType (undefined :: TitleArtist)) >>=
  fmap join . mapM (G.getTargetsByType (undefined :: Artist)) >>=
  mapM G.getValue

getStats :: G.Read Catalogue s (Int, Int)
getStats = G.getStats

-----------
-- Boilerplate.
-----------

G.generateBoilerplate ''Catalogue

-----------
-- QuickCheck stuff.
-----------

newtype Update a = Update (ReaderT (G.Engine Catalogue) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (G.Engine Catalogue))

runUpdate :: Update a -> G.Engine Catalogue -> IO a
runUpdate (Update readerT) db = runReaderT readerT db

instance Show (Update ()) where
  show _ = "Update ()"

instance QC.Arbitrary (Update ()) where
  arbitrary = 
    fmap Update $ QC.runGenT $ QC.oneof $
      replicate 2 addArtist ++
      replicate 6 addRelease ++
      replicate 60 addRecording ++
      replicate 1 removeArtist ++
      []
    where

      addArtist = do
        artist <- do
          uid <- runEvent GenerateNewUID
          name <- arbitrary
          return $ Artist uid name
        runEvent $ LinkCatalogueToArtist artist
        do
          releases <- arbitraryElements =<< runEvent GetAllReleases
          forM_ releases $ \release -> runEvent $ LinkArtistToRelease artist release
        do
          recordings <- arbitraryElements =<< runEvent GetAllRecordings
          forM_ recordings $ \recording -> runEvent $ LinkArtistToRecording artist recording

      addRelease = do
        release <- do
          uid <- runEvent GenerateNewUID
          title <- arbitrary
          typ <- QC.elements (enumFrom minBound)
          date <- arbitrary
          return $ Release uid title typ date
        runEvent $ LinkCatalogueToRelease release
        do
          artists <- arbitraryElements =<< runEvent GetAllArtists
          titleArtist <- TitleArtist <$> arbitrary
          forM_ artists $ \artist -> runEvent $ LinkReleaseToArtist release titleArtist artist
        do
          recordings <- arbitraryElements =<< runEvent GetAllRecordings
          forM_ (zip [1..] recordings) $ \(n, recording) -> do
            let track = Track n
            runEvent $ LinkReleaseToRecording release track recording

      addRecording = do
        recording <- do
          uid <- runEvent GenerateNewUID
          duration <- arbitrary
          typ <- QC.elements (enumFrom minBound)
          return $ Recording uid duration typ
        runEvent $ LinkCatalogueToRecording recording
        do
          artists <- arbitraryElements =<< runEvent GetAllArtists
          titleArtist <- TitleArtist <$> arbitrary
          forM_ artists $ \artist -> runEvent $ LinkRecordingToArtist recording titleArtist artist

      removeArtist = do
        artists <- return . listToMaybe =<< arbitraryElements =<< runEvent GetAllArtists
        forM_ artists $ \(Artist uid _) -> do
          runEvent $ RemoveArtistByUID uid

      runEvent :: 
        G.IsUnionEvent Catalogue e => 
        e -> QC.GenT (ReaderT (G.Engine Catalogue) IO) (G.EventResult Catalogue e)
      runEvent e = do
        db <- lift $ ask
        liftIO $ G.runEvent db e

arbitrary :: (QC.MonadGen g, QC.Arbitrary a) => g a
arbitrary = QC.liftGen QC.arbitrary

arbitraryElements :: (QC.MonadGen g, Eq a) => [a] -> g [a]
arbitraryElements list = case list of
  [] -> return []
  _ -> fmap nub $ QC.listOf $ QC.elements list

