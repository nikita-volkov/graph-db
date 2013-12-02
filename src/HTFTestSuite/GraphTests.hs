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

prop_deserializedGraphSerializesIntoTheSameByteString :: G.Graph Ints -> Property
prop_deserializedGraphSerializesIntoTheSameByteString graph = monadicIO $ do
  (a, n) <- run $ do
    bs <- serializeToBS graph
    n <- G.runRead graph $ G.getRoot >>= G.countTargets
    return (bs, n)

  pre (n > 0)

  b <- run $ do
    graph :: G.Graph Ints <- deserializeFromBS a
    serializeToBS graph

  assert $ a == b

  where
    serializeToBS = CerealPlus.Serialize.exec . serialize
    deserializeFromBS bs = 
      CerealPlus.Deserialize.runPartial deserialize bs >>= \r -> case r of
        CerealPlus.Deserialize.Done r _ -> return r
        _ -> error "Deserialization failure"

-- Required by QuickCheck.
instance Show (G.Graph Ints) where
  show graph = "Graph " <> show stats
    where
      stats = unsafePerformIO $ G.runRead graph G.getStats

instance Eq (G.Graph Ints) where
  a == b = error "TODO: testing graph equality"

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
  unionValueIndexHashes UnionValueType_Ints (UnionValue_Int v) =
    map hash (G.indexes v :: [G.Index Ints Ints Int])
  unionValueIndexHashes UnionValueType_Int (UnionValue_Int v) =
    map hash (G.indexes v :: [G.Index Ints Int Int])
  unionValueAny (UnionValue_Ints v) = unsafeCoerce v
  unionValueAny (UnionValue_Int v) = unsafeCoerce v
  unionValueType (UnionValue_Ints v) = UnionValueType_Ints
  unionValueType (UnionValue_Int v) = UnionValueType_Int
  unionValueTypeValue any UnionValueType_Ints = UnionValue_Ints (unsafeCoerce any)
  unionValueTypeValue any UnionValueType_Int = UnionValue_Int (unsafeCoerce any)
instance Hashable (G.UnionValueType Ints)
instance Serializable m (G.UnionValueType Ints)
instance Serializable m (G.UnionValue Ints)
instance Serializable m Ints
instance Hashable (G.Index Ints Ints Int)
instance Hashable (G.Index Ints Int Int)
instance G.IsUnionValue Ints Ints where
  toUnionValue v = UnionValue_Ints v
instance G.IsUnionValue Ints Int where
  toUnionValue v = UnionValue_Int v
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
          arbitraryTransaction graph = oneof [addRandom, addRandom, removeFirst]
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

