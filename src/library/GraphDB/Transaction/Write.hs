module GraphDB.Transaction.Write where

import GraphDB.Util.Prelude hiding (Write)
import qualified GraphDB.Persistence.Log as Log; import GraphDB.Persistence.Log (Log)
import qualified GraphDB.Engine.Node as Node; import GraphDB.Engine.Node (Node)
import qualified GraphDB.Util.DIOVector as DIOVector; import GraphDB.Util.DIOVector (DIOVector)

-- |
-- A monad performing the modification of data structure in IO and
-- accumulating all the information needed to be serialized.
newtype Write t s r = 
  Write (ActionsWriter t (EnvReader t IO) r)
  deriving (Monad, Functor, Applicative, MonadIO)

type EnvReader t = ReaderT (Env t)
type Env t = (Node t, DIOVector (Node t))
type ActionsWriter t = WriterT [Log.Action t]


-- |
-- Apply the transaction to the in-memory data-structure and
-- persist the updates if successful.
run :: Write t s r -> (Node t, Log t) -> IO r
run (Write writer) (root, log) = do
  refs <- DIOVector.new
  (r, actions) <- runReaderT (runWriterT writer) (root, refs)
  Log.persist log $ Log.Transaction actions
  return r


data Ref t s = Ref !Int !(Node t)


newRef :: Node t -> Write t s (Ref t s)
newRef node = Write $ do
  (_, refs) <- ask
  index <- liftIO $ DIOVector.append refs node
  return $ Ref index node

getRoot :: Write t s (Ref t s)
getRoot = do
  logAction $ Log.GetRoot
  (root, _) <- Write $ ask
  newRef root

getTargetsByType :: (Node.Type t) => Ref t s -> t -> Write t s [Ref t s]
getTargetsByType (Ref index node) t = do
  logAction $ Log.GetTargetsByType index t
  targets <- liftIO $ Node.getTargetsByType node t
  mapM newRef targets

newNode :: (Node.Type t) => Node.Value t -> Write t s (Ref t s)
newNode value = do
  logAction $ Log.NewNode value
  node <- liftIO $ Node.new value
  newRef node

addTarget :: (Node.Type t) => Ref t s -> Ref t s -> Write t s Bool
addTarget (Ref taIn ta) (Ref soIn so) = do
  logAction $ Log.AddTarget taIn soIn
  liftIO $ Node.addTarget ta so

logAction :: Log.Action t -> Write t s ()
logAction a = Write $ tell [a]
