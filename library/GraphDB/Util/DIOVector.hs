module GraphDB.Util.DIOVector where

import GraphDB.Util.Prelude
import qualified Data.Vector.Mutable as IOVector

-- | Dynamic mutable vector in 'IO'.
newtype DIOVector a = DIOVector (IORef (IOVector.IOVector a, Int))

new :: IO (DIOVector a)
new = newSized (2^8)

newSized :: Int -> IO (DIOVector a)
newSized size = do
  vector <- IOVector.new size
  ref <- newIORef (vector, 0)
  return $ DIOVector ref

-- | Append an item and return its index.
append :: DIOVector a -> a -> IO Int
append (DIOVector ref) value = do
  (vector, nextIndex) <- readIORef ref
  let size = IOVector.length vector
  vector' <- 
    if nextIndex >= size
      then IOVector.grow vector size
      else return vector
  IOVector.write vector' nextIndex value
  writeIORef ref (vector', succ nextIndex)
  return nextIndex

lookup :: DIOVector a -> Int -> IO (Maybe a)
lookup v i = do
  s <- size v
  if i < s && i >= 0
    then Just <$> unsafeLookup v i
    else pure Nothing

unsafeLookup :: DIOVector a -> Int -> IO a
unsafeLookup (DIOVector ref) i = do
  (vector, _) <- readIORef ref
  IOVector.read vector i

size :: DIOVector a -> IO Int
size (DIOVector ref) = readIORef ref >>= return . snd

