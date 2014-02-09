module GraphDB.Transaction.Write where

import GraphDB.Util.Prelude hiding (Write)
import qualified GraphDB.Persistence.Log.Transaction as LogTransaction
import qualified GraphDB.Persistence.Log as Log; import GraphDB.Persistence.Log (Log)
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)


-- |
-- A monad performing the modification of data structure in IO and
-- accumulating all the information needed to be serialized.
newtype Write t s r = 
  Write (ActionsWriter t (EnvReader t IO) r)
  deriving (Monad, Functor, Applicative, MonadIO)

type EnvReader t = ReaderT (Env t)
type Env t = (Node t, IORef Int)
type ActionsWriter t = WriterT [LogTransaction.Action t]


-- |
-- Apply the transaction to the in-memory data-structure and
-- persist the updates if successful.
run :: Write t s r -> (Node t, Log t) -> IO r
run (Write writer) (root, log) = do
  index <- newIORef 0
  (r, actions) <- runReaderT (runWriterT writer) (root, index)
  Log.persist log $ LogTransaction.Transaction actions
  return r


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
  logAction $ LogTransaction.GetRoot
  (root, _) <- Write $ ask
  newRef root

getTargetsByType :: (Node.Type t) => Ref t s -> t -> Write t s [Ref t s]
getTargetsByType (Ref index node) t = do
  logAction $ LogTransaction.GetTargetsByType index t
  targets <- liftIO $ Node.getTargetsByType node t
  mapM newRef targets

newNode :: (Node.Type t) => Node.Value t -> Write t s (Ref t s)
newNode value = do
  logAction $ LogTransaction.NewNode value
  node <- liftIO $ Node.new value
  newRef node

addTarget :: (Node.Type t) => Ref t s -> Ref t s -> Write t s Bool
addTarget (Ref taIn ta) (Ref soIn so) = do
  logAction $ LogTransaction.AddTarget taIn soIn
  liftIO $ Node.addTarget ta so

logAction :: LogTransaction.Action t -> Write t s ()
logAction a = Write $ tell [a]
