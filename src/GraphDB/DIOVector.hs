module GraphDB.DIOVector where

import GraphDB.Prelude
import qualified Data.Vector.Mutable as IOVector

-- | Dynamic mutable vector in 'IO'.
newtype DIOVector a = DIOVector (MVar (IOVector.IOVector a, Int))

new :: IO (DIOVector a)
new = do
  vector <- IOVector.new (2^8)
  var <- newMVar (vector, 0)
  return $ DIOVector var

append :: DIOVector a -> a -> IO ()
append (DIOVector var) value = 
  modifyMVar_ var $ \(vector, nextIndex) -> do
    let length = IOVector.length vector
    vector' <- 
      if nextIndex >= length
        then IOVector.grow vector length
        else return vector
    IOVector.write vector' nextIndex value
    return (vector', succ nextIndex)

unsafeLookup :: DIOVector a -> Int -> IO a
unsafeLookup = error "TODO: GraphDB.DIOVector.unsafeLookup"

length :: DIOVector a -> IO Int
length (DIOVector var) = readMVar var >>= return . snd

