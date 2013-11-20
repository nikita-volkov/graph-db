module GraphDB.IOStableNameSet where

import GraphDB.Prelude
import qualified Data.HashTable.IO as Table


newtype IOStableNameSet a = IOStableNameSet (Table.BasicHashTable (StableName a) a)


new :: IO (IOStableNameSet a)
new = IOStableNameSet <$> Table.new

insert :: IOStableNameSet a -> a -> IO ()
insert (IOStableNameSet t) a = do
  sn <- makeStableName a
  Table.insert t sn a

delete :: IOStableNameSet a -> a -> IO ()
delete (IOStableNameSet t) a = do
  sn <- makeStableName a
  Table.delete t sn

lookup :: IOStableNameSet a -> a -> IO Bool
lookup (IOStableNameSet t) = makeStableName >=> Table.lookup t >=> return . isJust



