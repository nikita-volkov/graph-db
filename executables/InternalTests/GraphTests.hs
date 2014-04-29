{-# OPTIONS_GHC -F -pgmF htfpp #-}
module InternalTests.GraphTests where

import GraphDB.Util.Prelude
import Test.Framework hiding (frequency, oneof, listOf, elements, choose)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances ()
import QuickCheck.GenT
import GraphDB.Graph
import qualified GraphDB.Util.DIOVector as V
import qualified CerealPlus.Serialize as CS
import qualified CerealPlus.Deserialize as CD


-- * Model
-------------------------

type Catalogue = ()
type Name = Text
type UID = Int

instance Setup Catalogue where
  type Algorithm Catalogue = Basic
  data Index Catalogue = 
    Catalogue_Artist_UID UID |
    Catalogue_Artist_Name Name |
    Catalogue_Genre_Name Name |
    Catalogue_Song_Name Name |
    Genre_Song |
    Song_Artist
    deriving (Eq, Generic)
  data Value Catalogue =
    Catalogue Catalogue |
    Artist UID Name |
    Genre Name |
    Song Name
    deriving (Eq, Generic)
  indexes to from = case (to, from) of
    (Artist uid n, Catalogue _) -> [Catalogue_Artist_Name n, Catalogue_Artist_UID uid]
    (Artist uid n, Song _) -> [Song_Artist]
    (Genre n, Catalogue _) -> [Catalogue_Genre_Name n]
    (Song n, Catalogue _) -> [Catalogue_Song_Name n]
    (Song n, Genre _) -> [Genre_Song]
    _ -> []

instance Serializable m (Index Catalogue)
instance Serializable m (Value Catalogue)
instance Hashable (Index Catalogue)
instance Hashable (Value Catalogue)


-- * QuickCheck
-------------------------

newtype Update = Update (ReaderT (Node Catalogue) IO ())

instance Show Update where
  show _ = "<Update>"

instance Arbitrary Update where
  arbitrary = fmap Update $ runGenT $ frequency
    [
      (50, addSomeEdge),
      (30, removeSomeEdge),
      (40, insertArtist),
      (20, insertGenre),
      (200, insertSong)
    ]
    where
      removeSomeEdge = do
        root <- lift $ ask
        void $ runMaybeT $ do
          source <- MaybeT $ oneof [selectSomeNode, return $ Just root]
          target <- MaybeT $ selectSomeNode
          liftIO $ removeTarget source target
      addSomeEdge = do
        void $ runMaybeT $ do
          source <- MaybeT selectSomeNode
          target <- MaybeT selectSomeNode
          liftIO $ addTarget source target
      selectSomeNode = do
        root <- lift $ ask
        targets <- liftIO $ V.new
        liftIO $ traverseTargets root $ void . V.append targets
        size <- liftIO $ V.size targets
        if size > 0
          then do
            index <- liftGen $ choose (0, size - 1)
            fmap Just $ liftIO $ V.unsafeLookup targets index
          else
            return Nothing
      insertArtist = addValueToCatalogue =<< do liftGen $ Artist <$> arbitrary <*> arbitrary
      insertGenre = addValueToCatalogue =<< do liftGen $ Genre <$> arbitrary
      insertSong = addValueToCatalogue =<< do liftGen $ Song <$> arbitrary
      addValueToCatalogue value = do
        source <- lift $ ask
        target <- liftIO . new $ value
        liftIO $ addTarget source target


-- * Tests
-------------------------

test_remove = do
  catalogue  <- new $ Catalogue ()
  michael    <- new $ Artist 1 "Michael Jackson"
  billieJean <- new $ Song "Billie Jean"
  whoIsIt    <- new $ Song "Who is it?"
  
  addTarget catalogue  michael
  addTarget catalogue  billieJean
  addTarget catalogue  whoIsIt
  addTarget billieJean michael
  addTarget whoIsIt    michael

  remove michael

  assertEqual (3, 2, 2) =<< getStats catalogue
  assertEqual 0 . length =<< getSources michael

test_stats = do
  catalogue  <- new $ Catalogue ()
  michael    <- new $ Artist 1 "Michael Jackson"
  billieJean <- new $ Song "Billie Jean"
  whoIsIt    <- new $ Song "Who is it?"
  
  addTarget catalogue  michael
  addTarget catalogue  billieJean
  addTarget catalogue  whoIsIt
  addTarget billieJean michael
  addTarget whoIsIt    michael

  assertEqual (4, 5, 6) =<< getStats catalogue

test_addingANodeAffectsTheStats = do
  root <- new $ Catalogue ()
  addTarget root =<< do new $ Artist 1 "Michael Jackson"
  assertEqual (2, 1, 2) =<< getStats root

test_removingANodeAffectsTheStats = do
  root <- new $ Catalogue ()
  artist <- new $ Artist 1 "Michael Jackson"
  addTarget root artist
  removeTarget root artist
  assertEqual (1, 0, 0) =<< getStats root

test_addingATargetTwiceMakesNoDifference = do
  root <- new $ Catalogue ()
  artist <- new $ Artist 1 "Michael Jackson"
  addTarget root artist
  addTarget root artist
  assertEqual (2, 1, 2) =<< getStats root

test_traverseTargetsDoesNotRepeat = do
  root <- new $ Catalogue ()
  addTarget root =<< do new $ Artist 1 "Michael Jackson"
  counter <- newIORef 0
  traverseTargets root $ const $ modifyIORef counter succ
  assertEqual 1 =<< readIORef counter

test_traverseSourcesDoesNotRepeat = do
  root <- new $ Catalogue ()
  artist <- new $ Artist 1 "Michael Jackson"
  addTarget root artist
  counter <- newIORef 0
  traverseSources artist $ const $ modifyIORef counter succ
  assertEqual 1 =<< readIORef counter

prop_serializeDeserializePreservesStats = monadicIO $ do
  node <- do
    updates :: [Update] <- pick $ do
      amount <- choose (0, 100)
      replicateM amount arbitrary
    run $ do
      root <- new $ Catalogue ()
      forM_ updates $ \(Update u) -> runReaderT u root
      return root 

  stats <- run $ getStats node
  run $ traceIO $ "Stats: " <> show stats

  bs <- run $ CS.exec $ serialize $ node
  CD.Done (node' :: Node Catalogue) _ <- run $ CD.runPartial deserialize bs
  bs' <- run $ CS.exec $ serialize $ node'
  stats' <- run $ getStats node'

  assert $ stats == stats'
