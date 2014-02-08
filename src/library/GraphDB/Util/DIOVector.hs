module GraphDB.Util.DIOVector where

import GraphDB.Util.Prelude
import qualified Data.Vector.Mutable as IOVector

-- | Dynamic mutable vector in 'IO'.
newtype DIOVector a = DIOVector (IORef (IOVector.IOVector a, Int))

new :: IO (DIOVector a)
new = do
  vector <- IOVector.new (2^8)
  ref <- newIORef (vector, 0)
  return $ DIOVector ref

append :: DIOVector a -> a -> IO ()
append (DIOVector ref) value = do
  (vector, nextIndex) <- readIORef ref
  let size = IOVector.length vector
  vector' <- 
    if nextIndex >= size
      then IOVector.grow vector size
      else return vector
  IOVector.write vector' nextIndex value
  writeIORef ref (vector', succ nextIndex)

lookup :: DIOVector a -> Int -> IO (Maybe a)
lookup v i = do
  size <- getSize v
  if i < size && i >= 0
    then Just <$> unsafeLookup v i
    else pure Nothing

unsafeLookup :: DIOVector a -> Int -> IO a
unsafeLookup (DIOVector ref) i = do
  (vector, _) <- readIORef ref
  IOVector.read vector i

getSize :: DIOVector a -> IO Int
getSize (DIOVector ref) = readIORef ref >>= return . snd

