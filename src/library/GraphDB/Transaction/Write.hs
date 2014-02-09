module GraphDB.Transaction.Write where

import GraphDB.Util.Prelude hiding (Write)
import qualified GraphDB.Transaction.Log as Log
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)


-- |
-- A monad performing the modification of data structure in IO and
-- accumulating all the information needed to be serialized.
newtype Write t s r = 
  Write (LogEntriesWriter t (EnvReader t IO) r)
  deriving (Monad, Functor, Applicative, MonadIO)

type EnvReader t = ReaderT (Env t)
type Env t = (Node t, IORef Int)
type LogEntriesWriter t = WriterT [Log.Entry t]


-- |
-- Apply the transaction to the in-memory data-structure and
-- accumulate the modification log.
run :: Write t s r -> Node t -> IO (r, Log.Log t)
run (Write writer) root = do
  index <- newIORef 0
  (r, entries) <- runReaderT (runWriterT writer) (root, index)
  let log = Log.Log entries
  return (r, log)


data Ref t s = Ref !Int !(Node t)


newRef :: Node t -> Write t s (Ref t s)
newRef node = Write $ do
  index <- do
    (_, ref) <- ask
    value <- liftIO $ do
      value <- readIORef ref
      modifyIORef' ref succ
      return value
    return value
  return $ Ref index node

getRoot :: Write t s (Ref t s)
getRoot = do
  logEntry $ Log.GetRoot
  (root, _) <- Write $ ask
  newRef root

getTargetsByType :: (Node.Type t) => Ref t s -> t -> Write t s [Ref t s]
getTargetsByType (Ref index node) t = do
  logEntry $ Log.GetTargetsByType index t
  targets <- liftIO $ Node.getTargetsByType node t
  mapM newRef targets

newNode :: (Node.Type t) => Node.Value t -> Write t s (Ref t s)
newNode value = do
  logEntry $ Log.NewNode value
  node <- liftIO $ Node.new value
  newRef node

addTarget :: (Node.Type t) => Ref t s -> Ref t s -> Write t s Bool
addTarget (Ref taIn ta) (Ref soIn so) = do
  logEntry $ Log.AddTarget taIn soIn
  liftIO $ Node.addTarget ta so

logEntry :: Log.Entry t -> Write t s ()
logEntry a = Write $ tell [a]
