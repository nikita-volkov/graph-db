module GraphDB.Transaction.Write where

import GraphDB.Util.Prelude hiding (Write)
import qualified GraphDB.Transaction.Log as Log
import qualified GraphDB.Union as Union
import qualified GraphDB.Engine.Node as Node




-- |
-- A monad performing the modification of a data structure in IO and
-- accumulating all the information needed to be serialized.
newtype Write union state result = 
  Write (LogEntriesWriter union (EnvReader union IO) result)
  deriving (Monad, Functor, Applicative, MonadIO)

type EnvReader u = ReaderT (Env u)
type Env u = (Union.Node u, IORef Int)
type LogEntriesWriter u = WriterT [Log.Entry u]


-- |
-- Apply the transaction to the in-memory data-structure and
-- accumulate the modifications log.
run :: Write u s r -> Union.Node u -> IO (r, Log.Log u)
run (Write writer) root = do
  index <- newIORef 0
  (r, entries) <- runReaderT (runWriterT writer) (root, index)
  let log = Log.Log entries
  return (r, log)


-- |
-- A transaction-local index of the node used for serialization
-- and the node itself. 
data Ref u s = Ref !Int !(Union.Node u)


newRef :: Union.Node u -> Write u s (Ref u s)
newRef node = Write $ do
  index <- do
    (_, ref) <- ask
    value <- liftIO $ do
      value <- readIORef ref
      modifyIORef' ref succ
      return value
    return value
  return $ Ref index node

getRoot :: Write u s (Ref u s)
getRoot = do
  logEntry $ Log.GetRoot
  newRef =<< do
    (root, _) <- Write $ ask
    return root

getTargetsByType :: Union.Union u => Ref u s -> Union.Type u -> Write u s [Ref u s]
getTargetsByType (Ref index node) t = do
  logEntry $ Log.GetTargetsByType index t
  mapM newRef =<< do liftIO $ Node.getTargetsByType node t

newNode :: Union.Union u => Union.Value u -> Write u s (Ref u s)
newNode value = do
  logEntry $ Log.NewNode value
  newRef =<< do liftIO $ Node.new value

addTarget :: Union.Union u => Ref u s -> Ref u s -> Write u s Bool
addTarget (Ref taIn ta) (Ref soIn so) = do
  logEntry $ Log.AddTarget taIn soIn
  liftIO $ Node.addTarget ta so

logEntry :: Log.Entry u -> Write u s ()
logEntry a = Write $ tell [a]
