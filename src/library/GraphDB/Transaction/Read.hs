module GraphDB.Transaction.Read where

import GraphDB.Util.Prelude hiding (Read)
import qualified GraphDB.Union as Union
import qualified GraphDB.Engine.Node as Node


newtype Read union state result = 
  Read (ReaderT (Union.Node union) IO result)
  deriving (Monad, Functor, Applicative, MonadIO)


run :: Read u s r -> Union.Node u -> IO r
run (Read reader) root = runReaderT reader root


newtype Ref u s = Ref (Union.Node u)


newRef :: Union.Node u -> Read u s (Ref u s)
newRef node = return $ Ref node

getRoot :: Read u s (Ref u s)
getRoot = Read ask >>= newRef

getTargetsByType :: (Union.Union u) => Ref u s -> Union.Type u -> Read u s [Ref u s]
getTargetsByType (Ref node) t = do
  targets <- liftIO $ Node.getTargetsByType node t
  mapM newRef targets
