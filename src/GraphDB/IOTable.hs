-- TODO:
-- Could be optimized a bit by not implementing 'getSize', since we never use it,
-- and using a Bool for 'getNull' instead.
module GraphDB.IOTable where

import GraphDB.Prelude
import qualified Data.HashTable.IO as HashTables


data IOTable k v = IOTable (HashTables.LinearHashTable k v) (IORef Int)

new :: IO (IOTable k v)
new = IOTable <$> HashTables.new <*> newIORef 0

getNull :: IOTable k v -> IO Bool
getNull = getSize >=> return . (<= 0)

getSize :: IOTable k v -> IO Int
getSize (IOTable _ r) = readIORef r

insert :: (Hashable k, Eq k) => IOTable k v -> k -> v -> IO ()
insert (IOTable t sr) k v = HashTables.insert t k v >> modifyIORef sr succ

delete :: (Hashable k, Eq k) => IOTable k v -> k -> IO ()
delete (IOTable t sr) k = do
  HashTables.lookup t k >>= \case
    Nothing -> return ()
    Just _ -> HashTables.delete t k >> modifyIORef sr pred

lookup :: (Hashable k, Eq k) => IOTable k v -> k -> IO (Maybe v)
lookup (IOTable t _) = HashTables.lookup t

foldM :: IOTable k v -> a -> (a -> (k, v) -> IO a) -> IO a
foldM (IOTable t _) z f = HashTables.foldM f z t

forM_ :: IOTable k v -> ((k, v) -> IO b) -> IO ()
forM_ (IOTable t _) f = HashTables.mapM_ f t

getList :: (Hashable k, Eq k) => IOTable k v -> IO [(k, v)]
getList (IOTable t _) = HashTables.toList t
