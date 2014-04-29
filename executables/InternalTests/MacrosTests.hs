{-# OPTIONS_GHC -F -pgmF htfpp #-}
module InternalTests.MacrosTests where

import Test.Framework
import GraphDB.Util.Prelude
import qualified GraphDB.Util.Prelude.TH as TH
import qualified GraphDB.Model as G
import qualified GraphDB.Macros as G


type Catalogue = (UID Artist, UID Genre, UID Song)
newtype UID a = UID Int deriving (Show, Eq, Ord, Generic, Data, Typeable, Enum, Num, Real, Integral)
data Artist = Artist Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Genre = Genre Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Song = Song Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
type Name = Text
data Identified a = Identified {-# UNPACK #-} !(UID a) !a deriving (Show, Eq, Ord, Generic, Data, Typeable)


-- Edges
-------------------------

instance G.Edge Catalogue (Identified Artist) where
  data Index Catalogue (Identified Artist) =
    Catalogue_Artist_UID (UID Artist) |
    Catalogue_Artist_Name Text
    deriving (Eq, Generic)
  indexes (Identified uid (Artist n)) = 
    [Catalogue_Artist_UID uid, Catalogue_Artist_Name n]

instance G.Edge Catalogue (Identified Genre) where
  data Index Catalogue (Identified Genre) = 
    Catalogue_Genre_UID (UID Genre) |
    Catalogue_Genre_Name Text
    deriving (Eq, Generic)
  indexes (Identified uid (Genre n)) = 
    [Catalogue_Genre_UID uid, Catalogue_Genre_Name n]

instance G.Edge (Identified Genre) (Identified Song) where
  data Index (Identified Genre) (Identified Song) =
    Genre_Song
    deriving (Eq, Generic)
  indexes _ = [Genre_Song]

instance G.Edge (Identified Song) (Identified Artist) where
  data Index (Identified Song) (Identified Artist) =
    Song_Artist
    deriving (Eq, Generic)
  indexes _ = [Song_Artist]


-- Boilerplate
-------------------------

G.deriveSetup ''Catalogue
instance (Hashable a) => Hashable (UID a)
instance (Serializable m a) => Serializable m (UID a)
