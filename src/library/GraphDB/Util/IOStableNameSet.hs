module GraphDB.Util.IOStableNameSet
  (IOStableNameSet,
   new,
   insert,
   delete,
   lookup,
   foldM,
   forM_,
   getSize,
   getNull,
   getList)
  where

import GraphDB.Util.Prelude hiding (insert, delete, lookup, foldM, forM_)
import qualified Data.HashTable.IO as Table


data IOStableNameSet a = IOStableNameSet !(Table.LinearHashTable (StableName a) a) !(IORef Int)

new :: IO (IOStableNameSet a)
new = IOStableNameSet <$> Table.new <*> newIORef 0

-- |
-- Returns a boolean signifying whether an element was inserted,
-- which would be false, if the element existed already.
insert :: IOStableNameSet a -> a -> IO Bool
insert (IOStableNameSet table sizeRef) a = do
  sn <- makeStableName a
  Table.lookup table sn >>= \case
    Just _ -> return False
    Nothing -> do
      Table.insert table sn a
      modifyIORef sizeRef succ
      return True

delete :: IOStableNameSet a -> a -> IO Bool
delete (IOStableNameSet table sizeRef) a = do
  sn <- makeStableName a
  Table.lookup table sn >>= \case
    Just _ -> do
      Table.delete table sn
      modifyIORef sizeRef pred
      return True
    Nothing -> return False

lookup :: IOStableNameSet a -> a -> IO Bool
lookup (IOStableNameSet table sizeRef) = makeStableName >=> Table.lookup table >=> return . isJust

getSize :: IOStableNameSet a -> IO Int
getSize (IOStableNameSet table sizeRef) = readIORef sizeRef

foldM :: IOStableNameSet a -> z -> (z -> a -> IO z) -> IO z
foldM (IOStableNameSet table sizeRef) z f = Table.foldM f' z table
  where f' z (_, a) = f z a

getNull :: IOStableNameSet a -> IO Bool
getNull = getSize >=> return . (<= 0)

getList :: IOStableNameSet a -> IO [a]
getList sns = foldM sns [] (\li el -> return $ el : li)

forM_ :: IOStableNameSet a -> (a -> IO ()) -> IO ()
forM_ sns f = foldM sns () (\() el -> f el)
