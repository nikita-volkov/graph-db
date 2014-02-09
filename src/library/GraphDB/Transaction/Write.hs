module GraphDB.Transaction.Write where

import GraphDB.Util.Prelude hiding (Write)
import qualified GraphDB.Transaction.Log as Log
import qualified GraphDB.Transaction.Node as Node


-- |
-- A monad performing the modification of data structure in IO and
-- accumulating all the information needed to be serialized.
newtype Write node state result = 
  Write (LogEntriesWriter node (EnvReader node IO) result)
  deriving (Monad, Functor, Applicative, MonadIO)

type EnvReader n = ReaderT (Env n)
type Env n = (n, IORef Int)
type LogEntriesWriter n = WriterT [Log.Entry n]


-- |
-- Apply the transaction to the in-memory data-structure and
-- accumulate the modification log.
run :: Write n s r -> n -> IO (r, Log.Log n)
run (Write writer) root = do
  index <- newIORef 0
  (r, entries) <- runReaderT (runWriterT writer) (root, index)
  let log = Log.Log entries
  return (r, log)


data Ref n s = Ref !Int !n


newRef :: n -> Write n s (Ref n s)
newRef node = Write $ do
  index <- do
    (_, ref) <- ask
    value <- liftIO $ do
      value <- readIORef ref
      modifyIORef' ref succ
      return value
    return value
  return $ Ref index node

getRoot :: Write n s (Ref n s)
getRoot = do
  logEntry $ Log.GetRoot
  newRef =<< do
    (root, _) <- Write $ ask
    return root

getTargetsByType :: Node.Node n => Ref n s -> Node.Type n -> Write n s [Ref n s]
getTargetsByType (Ref index node) t = do
  logEntry $ Log.GetTargetsByType index t
  mapM newRef =<< do liftIO $ Node.getTargetsByType node t

newNode :: Node.Node n => Node.Value n -> Write n s (Ref n s)
newNode value = do
  logEntry $ Log.NewNode value
  newRef =<< do liftIO $ Node.new value

addTarget :: Node.Node n => Ref n s -> Ref n s -> Write n s Bool
addTarget (Ref taIn ta) (Ref soIn so) = do
  logEntry $ Log.AddTarget taIn soIn
  liftIO $ Node.addTarget ta so

logEntry :: Log.Entry n -> Write n s ()
logEntry a = Write $ tell [a]
