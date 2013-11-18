{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.GenerateBoilerplateTests where

import Test.Framework
import Language.Haskell.TH
import GraphDB.Prelude hiding (assert, elements)
import GraphDB.GenerateBoilerplate
import qualified GraphDB.API as API



writeEvent1 :: Artist -> [Genre] -> API.Write Catalogue s ()
writeEvent1 artist genreList = undefined

readEvent1 :: Text -> API.Read Catalogue s [Genre]
readEvent1 name = undefined


data Catalogue
data Artist = Artist {artistName :: Text} deriving (Show, Eq)
data Genre = Genre {genreName :: Text} deriving (Show, Eq)
data instance API.EdgeTo Artist = ArtistOf | ArtistOfByName Text deriving (Show, Eq, Generic)
data instance API.EdgeTo Genre = GenreOf | GenreOfByGenre Genre deriving (Show, Eq, Generic)

data WriteEvent1 = WriteEvent1 Artist [Genre]

decsQToStringsIO decsQ = 
  runQ decsQ >>= 
  return . unwords . map (unwords . words . pprint)

test_generateEvent = do
  actual <- decsQToStringsIO $
    generateEvent (mkName "WriteEvent1") =<< sequence [[t|Artist|], [t|[Genre]|]] -- '
  assertEqual expected actual
  where
    expected = "data WriteEvent1 = WriteEvent1 !HTFTestSuite.GenerateBoilerplateTests.Artist !([HTFTestSuite.GenerateBoilerplateTests.Genre]) deriving (GHC.Classes.Eq, GHC.Generics.Generic)"

test_generateEventInstance = do
  actual <- decsQToStringsIO $ do
    eventType <- [t|WriteEvent1|]
    tagType <- [t|Catalogue|]
    argTypes <- sequence [[t|Artist|], [t|Genre|]]
    resultType <- [t|()|]
    generateEventInstance eventType tagType 'WriteEvent1 'writeEvent1 argTypes resultType True -- '
  assertEqual expected actual
  where
    expected = "instance GraphDB.API.Event HTFTestSuite.GenerateBoilerplateTests.WriteEvent1 HTFTestSuite.GenerateBoilerplateTests.Catalogue where type GraphDB.API.EventResult HTFTestSuite.GenerateBoilerplateTests.WriteEvent1 HTFTestSuite.GenerateBoilerplateTests.Catalogue = () GraphDB.API.eventTransaction (HTFTestSuite.GenerateBoilerplateTests.WriteEvent1 _0 _1) = GraphDB.API.Write (HTFTestSuite.GenerateBoilerplateTests.writeEvent1 _0 _1)"



