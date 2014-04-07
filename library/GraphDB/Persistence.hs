module GraphDB.Persistence where

import GraphDB.Util.Prelude

import qualified GraphDB.Action as A
import qualified GraphDB.Model.Union as U
import qualified GraphDB.Util.FileSystem as FS
import qualified GraphDB.Util.IOQueue as Q
import qualified GraphDB.Storage as S'
import qualified GraphDB.Graph as G
import qualified GraphDB.Util.DIOVector as DV
import qualified GraphDB.Persistence.Log as L


-- * Session
-------------------------

type Session u m = ReaderT (Storage u, Q.IOQueue) (G.Session u m)
type Storage u = S'.Storage (G.Node u) (L.Log u)


-- * Transaction
-------------------------

newtype Tx u m r = 
  Tx (StateT (L.Log u) (StateT Int (G.Session u m)) r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Tx u) where 
  lift = Tx . lift . lift . lift . lift

runTransaction :: (MonadBaseControl IO m, U.Serializable IO u) => Bool -> Tx u m r -> Session u m r
runTransaction write (Tx tx) = do
  (r, log) <- 
    lift $ G.runTransaction write $ flip evalStateT 0 $ flip runStateT [] $ tx
  when write $ do
    (storage, ioq) <- ask
    liftBase $ Q.enqueue ioq $ S'.persistEvent storage $ reverse log
  return r


-- * Action
-------------------------

type Action u = L.Action u

runAction :: (MonadBase IO m, U.Union u) => Action u m r -> Tx u m r
runAction = iterTM $ \case
  A.NewNode v c -> do
    record $ L.NewNode v
    ir <- Tx $ lift $ lift $ G.runAction $ A.newNode v
    r <- newRef ir
    c r
  where
    record e = Tx $ modify $ (:) e
    newRef n = Tx $ lift $ do
      index <- get
      modify succ
      return $ index

