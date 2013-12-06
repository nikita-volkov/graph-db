module GraphDB.SNMultiTable where

import GraphDB.Prelude hiding (insert, delete, lookup, foldM, traverse)
import qualified Data.HashTable.IO as HT


data SNMultiTable k v =
  SNMultiTable 
    {-# UNPACK #-}
    !(HT.LinearHashTable k (HT.LinearHashTable (StableName v) v))
    {-# UNPACK #-}
    !(IORef Int)

new :: IO (SNMultiTable k v)
new = SNMultiTable <$> HT.new <*> newIORef 0

getNull :: SNMultiTable k v -> IO Bool
getNull = getSize >=> return . (<= 0)

getSize :: SNMultiTable k v -> IO Int
getSize (SNMultiTable _ r) = readIORef r

insert :: (Hashable k, Eq k) => SNMultiTable k v -> (k, v) -> IO Bool
insert (SNMultiTable t sr) (k, v) = do
  updateTable >>= \case
    True -> do
      modifyIORef sr succ
      return True
    False -> return False
  where
    updateTable = do
      k' <- makeStableName v
      HT.lookup t k >>= \case
        Nothing -> do
          t' <- HT.new
          HT.insert t' k' v
          HT.insert t k t'
          return True
        Just t' -> do
          HT.lookup t' k' >>= \case
            Nothing -> do
              HT.insert t' k' v
              return True
            Just _ -> return False

delete :: (Hashable k, Eq k) => SNMultiTable k v -> (k, v) -> IO Bool
delete (SNMultiTable t sr) (k, v) = do
  updateTable >>= \case
    True -> do
      modifyIORef sr pred
      return True
    False -> return False
  where
    updateTable = do
      HT.lookup t k >>= \case
        Nothing -> return False
        Just t' -> do
          k' <- makeStableName v
          HT.lookup t' k' >>= \case
            Nothing -> return False
            Just _ -> do
              HT.delete t' k'
              return True

lookup :: (Hashable k, Eq k) => SNMultiTable k v -> (k, v) -> IO (Maybe v)
lookup (SNMultiTable t _) (k, v) = do
  HT.lookup t k >>= \case
    Nothing -> return Nothing
    Just t' -> do
      k' <- makeStableName v
      HT.lookup t' k'

foldM :: SNMultiTable k v -> z -> (z -> (k, v) -> IO z) -> IO z
foldM (SNMultiTable t _) z f = HT.foldM f' z t
  where
    f' z (k, t') = HT.foldM f'' z t'
      where
        f'' z (_, v) = f z (k, v)

traverse :: SNMultiTable k v -> ((k, v) -> IO ()) -> IO ()
traverse t f = foldM t () (\() el -> f el)

getList :: (Hashable k, Eq k) => SNMultiTable k v -> IO [(k, v)]
getList ta = foldM ta [] (\li el -> return $ el : li)

traverseKeys :: SNMultiTable k v -> (k -> IO ()) -> IO ()
traverseKeys (SNMultiTable ta _) fu = HT.foldM fu' () ta
  where
    fu' _ (ke, _) = fu ke
