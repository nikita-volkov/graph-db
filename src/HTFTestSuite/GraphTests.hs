{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.GraphTests where

import Test.Framework
import Test.QuickCheck.Monadic
import GraphDB.Prelude hiding (assert)
import qualified GraphDB.Graph as G
import qualified CerealPlus.Serialize
import qualified CerealPlus.Deserialize


-- Graph equality is based on refs, so it can't be used.
-- prop_serializeDeserialize = monadicIO $ do
--   graph :: Graph Int Int <- run $ head <$> sample' arbitrary
--   r <- run $ deserialize =<< serialize graph
--   assert $ Just graph == r

prop_deserializedGraphSerializesIntoTheSameByteString = monadicIO $ do
  a <- run $ do
    graph :: G.Graph Ints <- head <$> sample' arbitrary
    serializeToBS graph
  b <- run $ do
    graph :: G.Graph Ints <- deserializeFromBS $ a
    serializeToBS graph
  assert $ a == b
  where
    serializeToBS = CerealPlus.Serialize.exec . serialize
    deserializeFromBS bs = 
      CerealPlus.Deserialize.runPartial deserialize bs >>= \r -> case r of
        CerealPlus.Deserialize.Done r _ -> return r
        _ -> error "Deserialization failure"


data Ints = Ints deriving (Show, Eq, Generic)
instance G.GraphTag Ints where
  type Root Ints = Ints
  data UnionValueType Ints =
    UnionValueType_Ints |
    UnionValueType_Int
    deriving (Show, Eq, Generic)
  data UnionValue Ints =
    UnionValue_Ints Ints |
    UnionValue_Int Int
    deriving (Show, Eq, Generic)
  unionIndexHashes = undefined

instance Hashable (G.UnionValueType Ints)
instance Serializable m (G.UnionValueType Ints)
instance Serializable m (G.UnionValue Ints)
instance Serializable m Ints
instance Hashable (G.Index Ints Ints Int)
instance Hashable (G.Index Ints Int Int)

instance G.IsUnionValue Ints Ints where
  toUnionValueType _ = UnionValueType_Ints
  toUnionValue v = UnionValue_Ints v
  fromUnionValue z = case z of UnionValue_Ints v -> Just v; _ -> Nothing

instance G.IsUnionValue Ints Int where
  toUnionValueType _ = UnionValueType_Int
  toUnionValue v = UnionValue_Int v
  fromUnionValue z = case z of UnionValue_Int v -> Just v; _ -> Nothing

instance G.Reachable Ints Ints Int where
  data Index Ints Ints Int =
    Index_Ints_Int
    deriving (Show, Eq, Generic)
instance G.Reachable Ints Int Int where
  data Index Ints Int Int =
    Index_Int_Int
    deriving (Show, Eq, Generic)

instance Arbitrary (G.Graph Ints) where
  arbitrary = do
    updates <- listOf arbitraryUpdate
    return $ unsafePerformIO $ do
      graph <- G.new Ints
      mapM_ ($ graph) updates
      return graph
    where
      arbitraryUpdate = 
        promote $ \graph -> do
          transaction <- arbitraryTransaction graph
          return $ G.runWrite graph $ unsafeCoerce transaction
        where 
          arbitraryTransaction graph = oneof [addRandom, removeFirst]
            where
              randomValue :: Gen Int
              randomValue = elements [1..20]
              addRandom :: Gen (G.Write Ints s ())
              addRandom = do
                value <- randomValue
                return $ do
                  node <- G.newNode value
                  G.addTarget node =<< G.getRoot
              removeFirst :: Gen (G.Write Ints s ())
              removeFirst = do
                return $ do
                  root <- G.getRoot
                  nodeM <- return . listToMaybe =<< G.getTargetsByType (undefined :: Int) root
                  forM_ nodeM $ \node -> do
                    G.removeTarget node root
              update = undefined

