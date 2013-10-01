module TPM.GraphDB.GetT where

import TPM.GraphDB.Prelude
import Data.Serialize



newtype GetT m a = GetT { run :: ByteString -> m (Result a) }

instance (Monad m) => Monad (GetT m) where
  GetT runA >>= aToGetTB = GetT $ \bs -> do
    rA <- runA bs
    case rA of
      Fail msg -> return $ Fail msg
      Partial cont -> error "FIXME: Partial parsing unimplemented"
      Done a bs' -> case aToGetTB a of GetT runB -> runB bs'
  return a = GetT $ \bs -> return $ Done a bs

instance MonadTrans GetT where
  lift m = GetT $ \bs -> m >>= \a -> return $ Done a bs

instance (MonadIO m) => MonadIO (GetT m) where
  liftIO = lift . liftIO

instance (Monad m) => Applicative (GetT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Functor (GetT m) where
  fmap f = (>>= return . f)



class MonadGet m where 
  liftGet :: Get a -> m a

instance (Monad m) => MonadGet (GetT m) where 
  liftGet get = GetT $ \bs -> return $ runGetPartial get bs

instance MonadGet Get where
  liftGet = id


