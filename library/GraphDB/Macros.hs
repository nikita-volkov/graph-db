module GraphDB.Macros where
 
import GraphDB.Util.Prelude
import GraphDB.Util.Prelude.TH
import qualified GraphDB.Util.TH as THU
import qualified GraphDB.Graph as G
import qualified GraphDB.Model as M
import qualified GraphDB.Macros.Templates as T
import qualified GraphDB.Macros.Analysis as A


-- |
-- Generate all the boilerplate code by the type of the root node.
deriveSetup :: Name -> Q [Dec]
deriveSetup root = do
  -- The work is done in three phases:
  -- 1. While in the 'Q' monad, reify all the required information.
  -- 2. Leave the 'Q' monad and run a pure analysis on that information
  -- to produce settings for templates to render.
  -- 3. While still out of the 'Q' monad, render all the templates.
  -- 
  -- Such strategy allows to separate the concerns and
  -- exploit parallelism in the last two phases.
  edgePairs <- reifyEdgePairs
  return $ T.renderDecs $ A.decs rootType edgePairs
  where
    rootType = ConT root

-- |
-- Scan the current module for instance declarations of 'M.Edge' and
-- collect the pairs of the types they link.
reifyEdgePairs :: Q [(Type, Type)]
reifyEdgePairs = do
  instances <- THU.reifyLocalInstances
  return $ do
    (n, tl) <- instances
    guard $ n == ''M.Edge
    case tl of [a, b] -> return (a, b)
