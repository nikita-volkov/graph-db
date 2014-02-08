module GraphDB.Transaction.Read where

import GraphDB.Util.Prelude hiding (Read)
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)


newtype Read t s r = 
  Read (ReaderT (Node t) IO r)
  deriving (Monad, Functor, Applicative, MonadIO)


run :: Read t s r -> Node t -> IO r
run (Read reader) root = runReaderT reader root


newtype Ref t s = Ref (Node t)


newRef :: Node t -> Read t s (Ref t s)
newRef node = return $ Ref node

getRoot :: Read t s (Ref t s)
getRoot = Read ask >>= newRef

getTargetsByType :: (Node.Type t) => Ref t s -> t -> Read t s [Ref t s]
getTargetsByType (Ref node) t = do
  targets <- liftIO $ Node.getTargetsByType node t
  mapM newRef targets
