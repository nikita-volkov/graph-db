module GraphDB.Util.SNMultiTable where

import GraphDB.Util.Prelude hiding (insert, delete, lookup, foldM, traverse)
import qualified Data.HashTable.IO as HT


data SNMultiTable k v =
  SNMultiTable 
    {-# UNPACK #-}
    !(HT.LinearHashTable k (HT.LinearHashTable (StableName v) v))
    {-# UNPACK #-}
    !(IORef Int)

type Key k = (Hashable k, Eq k)

new :: IO (SNMultiTable k v)
new = SNMultiTable <$> HT.new <*> newIORef 0

getNull :: SNMultiTable k v -> IO Bool
getNull = getSize >=> return . (<= 0)

getSize :: SNMultiTable k v -> IO Int
getSize (SNMultiTable _ r) = readIORef r

insert :: Key k => SNMultiTable k v -> (k, v) -> IO Bool
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

delete :: Key k => SNMultiTable k v -> (k, v) -> IO Bool
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

lookup :: Key k => SNMultiTable k v -> (k, v) -> IO (Maybe v)
lookup (SNMultiTable t _) (k, v) = do
  HT.lookup t k >>= \case
    Nothing -> return Nothing
    Just t' -> do
      k' <- makeStableName v
      HT.lookup t' k'

lookupByKey :: Key k => SNMultiTable k v -> k -> IO [v]
lookupByKey (SNMultiTable t _) k =
  HT.lookup t k >>= \case
    Nothing -> return []
    Just t' -> HT.foldM (\li (_, v) -> return $ v : li) [] t'

foldM :: SNMultiTable k v -> z -> (z -> k -> v -> IO z) -> IO z
foldM (SNMultiTable t _) z f = HT.foldM f' z t
  where
    f' z (k, t') = HT.foldM f'' z t'
      where
        f'' z (_, v) = f z k v

traverse :: SNMultiTable k v -> (k -> v -> IO ()) -> IO ()
traverse t f = foldM t () (\() k v -> f k v)

traverseKeys :: SNMultiTable k v -> (k -> IO ()) -> IO ()
traverseKeys (SNMultiTable ta _) fu = HT.foldM fu' () ta
  where
    fu' _ (ke, _) = fu ke

getList :: Key k => SNMultiTable k v -> IO [(k, v)]
getList ta = foldM ta [] (\li ke va -> return $ (ke, va) : li)


