
import GraphDB.Util.Prelude
import qualified GraphDB.Util.Prelude.TH as T
import qualified Criterion.Main as C
import qualified GraphDB.Macros.Analysis as MA
import qualified GraphDB.Macros.Templates as MT


main = 
  C.defaultMain
    [
      C.bench "Analysis" $ C.nf (MA.decs root) edges,
      C.bench "Rendering" $ C.nf (MT.renderDecs) decs,
      C.bench "Analysis and rendering" $ C.nf (MT.renderDecs . MA.decs root) edges
    ]

root = T.ConT ''Int
edges = 
  -- Some random types
  let types = [ ''Int, ''Int8, ''Int16, ''Int32, ''Int64, ''Integer, 
                ''Word, ''Word8, ''Word16, ''Word32, ''Word64,
                ''Bool, ''Char, ''String, ''Text, ''ByteString ]
      in [(T.ConT a, T.ConT b) | a <- types, b <- types]
decs = MA.decs root edges
