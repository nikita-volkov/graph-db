module TPM.GraphDB.DIOVector where

import TPM.GraphDB.Prelude
import qualified Data.Vector.Mutable as IOVector

-- | Dynamic mutable vector in 'IO'.
newtype DIOVector a = DIOVector (MVar (IOVector.IOVector a, Int))

new :: IO (DIOVector a)
new = undefined

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
unsafeLookup = undefined


