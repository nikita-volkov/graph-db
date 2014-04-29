
import GraphDB.Util.Prelude
import qualified GraphDB.Util.Prelude.TH as T
import qualified CriterionPlus as C
import qualified GraphDB.Macros.Analysis as A


-- * Model
-------------------------

type Catalogue = (UID Artist, UID Genre, UID Song)
newtype UID a = UID Int deriving (Show, Eq, Ord, Generic, Data, Typeable, Enum, Num, Real, Integral)
data Artist = Artist Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Genre = Genre Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Song = Song Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
type Name = Text
data Identified a = Identified {-# UNPACK #-} !(UID a) !a deriving (Show, Eq, Ord, Generic, Data, Typeable)


-- * Benchmark
-------------------------

main = do
  C.benchmark $ do
    C.standoff "" $ do
      C.subject "" $ do
        C.whnfIO $ return analyse

analyse = 
  A.decs 
    (T.ConT ''Catalogue)
    [
      (T.ConT ''Catalogue, T.ConT ''Artist),
      (T.ConT ''Catalogue, T.ConT ''Genre),
      (T.ConT ''Catalogue, T.ConT ''Song),
      (T.ConT ''Genre, T.ConT ''Song),
      (T.ConT ''Song, T.ConT ''Artist)
    ]
