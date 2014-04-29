
import GraphDB.Util.Prelude
import qualified GraphDB.Util.Prelude.TH as T
import qualified Criterion.Main as C
import qualified GraphDB.Macros.Analysis as MA
import qualified GraphDB.Macros.Templates as MT


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

main = 
  C.defaultMain
    [
      C.bench "Analysis" $ C.nf (show . MA.decs root) edges,
      C.bench "Rendering" $ C.nf (show . MT.renderDecs) decs,
      C.bench "Analysis and rendering" $ C.nf (show . MT.renderDecs . MA.decs root) edges
    ]

root = T.ConT ''Catalogue
edges = 
  [
    (T.ConT ''Catalogue, T.ConT ''Artist),
    (T.ConT ''Catalogue, T.ConT ''Genre),
    (T.ConT ''Catalogue, T.ConT ''Song),
    (T.ConT ''Genre, T.ConT ''Song),
    (T.ConT ''Song, T.ConT ''Artist)
  ]
decs = MA.decs root edges
