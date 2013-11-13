{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.GraphTests where

import Test.Framework
import Test.QuickCheck.Monadic
import TPM.GraphDB.Prelude hiding (assert)
import TPM.GraphDB.Graph
import qualified HTFTestSuite.Model as Model
import qualified Acid.IO.SerializeM as SerializeM
import Pipes
import qualified Pipes.Prelude
import qualified Pipes.ByteString


-- Graph equality is based on refs, so it can't be used.
-- prop_serializeDeserialize = monadicIO $ do
--   graph :: Graph Int Int <- run $ head <$> sample' arbitrary
--   r <- run $ deserialize =<< serialize graph
--   assert $ Just graph == r

prop_deserializedGraphSerializesIntoTheSameByteString = monadicIO $ do
  a <- run $ do
    graph :: Graph Int Int <- head <$> sample' arbitrary
    serialize graph
  b <- run $ do
    graph :: Graph Int Int <- fmap fromJust . deserialize $ a
    serialize graph
  assert $ a == b

serialize :: SerializeM.SerializeM s IO => s -> IO LazyByteString
serialize s = Pipes.ByteString.toLazyM $ SerializeM.serializerProducer s

deserialize :: SerializeM.SerializeM s IO => LazyByteString -> IO (Maybe s)
deserialize lbs = do
  r <- runEitherT $ Pipes.Prelude.head $ Pipes.ByteString.fromLazy lbs >-> SerializeM.deserializerPipe
  either (\m -> error $ show $ "Deserialization failure: " <> m) return r


instance Arbitrary (Graph Int Int) where
  arbitrary = do
    transactions <- listOf arbitraryTransaction
    return $ unsafePerformIO $ do
      graph <- new 0
      traverse_ ($ graph) transactions
      return graph
    where
      arbitraryTransaction = promote $ \graph -> do
        writes :: [NodeRef Int Int s -> Write Int Int s (NodeRef Int Int s)] <- listOf arbitraryWrite
        return $ runWrite graph $ unsafeCoerce $ do 
          ref <- foldr (=<<) getRoot $ writes
          getValue ref
        where
          arbitraryWrite = promote $ \source -> do
            getTargetNode <- do
              randomNode <- arbitrary
              elements $ [return source, getRoot] ++ replicate 10 (newNode randomNode)
            edge <- elements [1..3]
            value <- elements [1..20]
            operation <- elements [setValueOp, insertEdgeOp, deleteEdgeOp]
            return $ do
              target <- getTargetNode
              operation source target edge value
            where
              setValueOp source target edge value = setValue source value >> return source
              insertEdgeOp source target edge value = insertEdge source edge target >> return target
              deleteEdgeOp source target edge value = deleteEdge source edge target >> return source

