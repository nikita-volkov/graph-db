module TPM.GraphDB.Prelude (
  module TPM.Prelude
  ) where


import TPM.Prelude
import qualified Data.Serialize as Cereal



-- | Required by 'WriterT'
instance Monoid Cereal.Put where
  mempty = return ()
  mappend a b = a >> b
