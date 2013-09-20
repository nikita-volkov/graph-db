module TPM.GraphDB.Transaction where

import TPM.Prelude hiding (Read, Write)
import qualified TPM.GraphDB.DB as DB; import TPM.GraphDB.DB (DB)
import qualified TPM.GraphDB.Node as Node; import TPM.GraphDB.Node (Node)
import qualified TPM.GraphDB.Dispatcher as Dispatcher; import TPM.GraphDB.Dispatcher (Dispatcher)



class MonadIO t => Transaction t where
  run :: DB -> t a -> IO a
  getRoot :: t (Node ())

newtype Write a = Write (Node () -> IO a)

instance Transaction Write where
  run (DB.DB root dispatcher) (Write rootToIO) = 
    Dispatcher.runWrite dispatcher io 
    where io = rootToIO root
  getRoot = Write $ return

instance MonadIO Write where
  liftIO io = Write $ const io

instance Monad Write where
  return a = Write $ const $ return a

newtype Read a = Read (Node () -> IO a)

instance Transaction Read where
  run (DB.DB root dispatcher) (Read rootToIO) = 
    Dispatcher.runRead dispatcher io 
    where io = rootToIO root
  getRoot = Read $ return

instance MonadIO Read where
  liftIO io = Read $ const io

instance Monad Read where
  return a = Read $ const $ return a



getValue :: Transaction t => Node v -> t v
getValue node = liftIO $ Node.getValue node

insertEdge :: Node.Edge s t -> Node s -> Node t -> Write ()
insertEdge edge source target = do
  liftIO $ Node.insertEdge edge target source

newNode :: a -> Write (Node a)
newNode a = liftIO $ Node.new a
