{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.MacrosTests where

import Test.Framework
import Language.Haskell.TH
import GraphDB.Prelude hiding (assert, elements)
import GraphDB.Macros
import qualified GraphDB.API as API



writeEvent1 :: Artist -> [Genre] -> API.Write Catalogue s ()
writeEvent1 artist genreList = undefined

readEvent1 :: Text -> API.Read Catalogue s [Genre]
readEvent1 name = undefined


data Catalogue
data Artist = Artist {artistName :: Text} deriving (Show, Eq)
data Genre = Genre {genreName :: Text} deriving (Show, Eq)

data WriteEvent1 = WriteEvent1 Artist [Genre]