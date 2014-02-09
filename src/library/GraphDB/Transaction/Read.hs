module GraphDB.Transaction.Read where

import GraphDB.Util.Prelude hiding (Read)
import qualified GraphDB.Transaction.Node as Node


newtype Read node state result = 
  Read (ReaderT node IO result)
  deriving (Monad, Functor, Applicative, MonadIO)


run :: Read n s r -> n -> IO r
run (Read reader) root = runReaderT reader root


newtype Ref n s = Ref n


newRef :: n -> Read n s (Ref n s)
newRef node = return $ Ref node

getRoot :: Read n s (Ref n s)
getRoot = Read ask >>= newRef

getTargetsByType :: (Node.Node n) => Ref n s -> Node.Type n -> Read n s [Ref n s]
getTargetsByType (Ref node) t = do
  targets <- liftIO $ Node.getTargetsByType node t
  mapM newRef targets
