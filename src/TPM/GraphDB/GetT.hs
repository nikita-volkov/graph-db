module TPM.GraphDB.GetT where

import TPM.GraphDB.Prelude
import qualified Data.Serialize.Get as Get



data Result m a = 
  Fail String |
  Partial (ByteString -> m (Result m a)) |
  Done a ByteString

newtype GetT m a = GetT { run :: ByteString -> m (Result m a) }

instance (Monad m) => Monad (GetT m) where
  GetT runA >>= aToGetTB = GetT $ \bs -> runA bs >>= aToMB where
    aToMB a = case a of
      Fail msg -> return $ Fail msg
      Partial cont -> return $ Partial $ \bs -> cont bs >>= aToMB
      Done a bs -> case aToGetTB a of GetT runB -> runB bs
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
  liftGet :: Get.Get a -> m a

instance (Monad m) => MonadGet (GetT m) where 
  liftGet get = GetT $ \bs -> return $ convertResult $ Get.runGetPartial get bs where
    convertResult r = case r of
      Get.Fail m -> Fail m
      Get.Partial cont -> Partial $ \bs -> return $ convertResult $ cont bs
      Get.Done a bs -> Done a bs

instance MonadGet Get.Get where
  liftGet = id


