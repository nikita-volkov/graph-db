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
data Artist = Artist {artistName :: Text} deriving (Show)
data Genre = Genre {genreName :: Text} deriving (Show)
data instance API.Edge () Artist = UnitToArtistEdge | UnitToArtistByNameEdge Text
data instance API.Edge Artist Genre = ArtistToGenreEdge
data instance API.Edge () Genre = UnitToGenreEdge | UnitToGenreByGenreEdge Genre

data WriteEvent1 = WriteEvent1 Artist [Genre]

decsQToStringsIO decsQ = 
  runQ decsQ >>= 
  return . unwords . map (unwords . words . pprint)

test_generateEvent = do
  actual <- decsQToStringsIO $
    generateEvent 'writeEvent1 =<< sequence [[t|Artist|], [t|[Genre]|]] -- '
  assertEqual expected actual
  where
    expected = "data WriteEvent1 = WriteEvent1 !MainTestSuite.GenerateBoilerplateTests.Artist !([MainTestSuite.GenerateBoilerplateTests.Genre]) deriving (GHC.Generics.Generic, GHC.Classes.Eq)"

test_generateEventInstance = do
  actual <- decsQToStringsIO $ do
    eventType <- [t|WriteEvent1|]
    tagType <- [t|Catalogue|]
    argTypes <- sequence [[t|Artist|], [t|Genre|]]
    resultType <- [t|()|]
    generateEventInstance eventType tagType 'WriteEvent1 'writeEvent1 argTypes resultType True -- '
  assertEqual expected actual
  where
    expected = "instance GraphDB.API.Event MainTestSuite.GenerateBoilerplateTests.WriteEvent1 MainTestSuite.GenerateBoilerplateTests.Catalogue where type GraphDB.API.EventResult MainTestSuite.GenerateBoilerplateTests.WriteEvent1 MainTestSuite.GenerateBoilerplateTests.Catalogue = () GraphDB.API.eventTransaction = \\(MainTestSuite.GenerateBoilerplateTests.WriteEvent1 _0 _1) -> GraphDB.API.Write (MainTestSuite.GenerateBoilerplateTests.writeEvent1 _0 _1)"



