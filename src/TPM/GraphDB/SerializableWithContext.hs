module TPM.GraphDB.SerializableWithContext where

import TPM.Prelude
import qualified Data.SafeCopy as SafeCopy; import Data.SafeCopy (SafeCopy)
import qualified Data.Serialize as Serialize



class SerializableWithContext a where
  type Context a
  put :: Serialize.Putter a
  get :: Context a -> Serialize.Get a

