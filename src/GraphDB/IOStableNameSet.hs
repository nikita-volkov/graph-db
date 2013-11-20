module GraphDB.IOStableNameSet where

import GraphDB.Prelude hiding (lookup, foldM)
import qualified Data.HashTable.IO as Table


data IOStableNameSet a = IOStableNameSet {
  insert :: a -> IO (),
  delete :: a -> IO (),
  lookup :: a -> IO Bool,
  getSize :: IO Int,
  foldM :: forall z. (z -> a -> IO z) -> z -> IO z
}

new :: forall a. IO (IOStableNameSet a)
new = fromEnv <$> newTable <*> newIORef 0
  where
    newTable = Table.new :: IO (Table.BasicHashTable (StableName a) a)
    fromEnv table sizeRef = IOStableNameSet insert delete lookup getSize foldM
      where
        insert a = do
          sn <- makeStableName a
          Table.lookup table sn >>= \case
            Just _ -> return ()
            Nothing -> do
              Table.insert table sn a
              modifyIORef sizeRef succ
        delete a = do
          sn <- makeStableName a
          Table.lookup table sn >>= \case
            Just _ -> do
              Table.delete table sn
              modifyIORef sizeRef pred
            Nothing -> return ()
        lookup = makeStableName >=> Table.lookup table >=> return . isJust
        getSize = readIORef sizeRef
        foldM :: (z -> a -> IO z) -> z -> IO z
        foldM f z = Table.foldM f' z table
          where f' z (_, a) = f z a

getNull :: IOStableNameSet a -> IO Bool
getNull = getSize >=> return . (<= 0)

toList :: IOStableNameSet a -> IO [a]
toList sns = foldM sns (\li el -> return $ el : li) []


