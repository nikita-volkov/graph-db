module TPM.GraphDB.Serialization where

import TPM.GraphDB.Prelude



class IsTerm a db where
  toTerm :: a -> Term db
  fromTerm :: Term db -> a

-- | 
-- A union type for all nodes, edges and events under a certain tag.
-- Used as a dictionary for deserialization.
-- Client specifies a 'SafeCopy' instance for it.
data family Term db
deriving instance Typeable1 Term



-- deserialize :: Cereal.Get (IO (Node db ()))
-- deserialize = do
--   undefined
--   where
--     populateNode :: Node db a -> 
--                     ReaderT (Table Int (forall a. Node db a)) Cereal.Get (IO ())
--     populateNode node = undefined
