{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.GenerateBoilerplate.TransactionReificationTests where

import Test.Framework
import GraphDB.Prelude hiding (assert, elements, Read)
import GraphDB.GenerateBoilerplate
import Language.Haskell.TH
import qualified GraphDB.API as API



type Read r = forall s. API.Read Char s r
type Read' s r = API.Read Char s r
type Write r = forall s. API.Write Char s r
type Write' s r = API.Write Char s r

transactionFunction1 :: Read' s Int
transactionFunction1 = undefined

transactionFunction2 :: Char -> Read' s Int
transactionFunction2 = undefined


test_reifyLocalTransactionFunctions = do
  assertElem "transactionFunction2" functionNames
  assertElem "transactionFunction1" functionNames
  where
    functionNames :: [String]
    functionNames = read $(
        reifyLocalTransactionFunctions (ConT ''Char) >>=
        stringE . show . map (\(n, _, _ , _) -> show n)
      )

test_isWrite = do
  assertBool $ read $(
      reifyTransaction (ConT ''Write `AppT` ConT ''Int) >>=
      stringE . show . (== Just True) . fmap (\(a, _, _) -> a)
    )
  assertBool $ read $(
      reifyTransaction (ConT ''Write' `AppT` ConT ''Int) >>=
      stringE . show . (== Just True) . fmap (\(a, _, _) -> a)
    )
  assertBool $ not $ read $(
      reifyTransaction (ConT ''Read `AppT` ConT ''Int) >>=
      stringE . show . (== Just True) . fmap (\(a, _, _) -> a)
    )
  assertBool $ not $ read $(
      reifyTransaction (ConT ''Read' `AppT` ConT ''Int) >>=
      stringE . show . (== Just True) . fmap (\(a, _, _) -> a)
    )

test_writeSynExpansion = do
  assertBool $ read $(
      stringE . show . isJust =<< 
      reifyTransaction (ConT ''Write `AppT` ConT ''Int)
    )

test_readSynExpansion = do
  assertBool $ read $(
      stringE . show . isJust =<< 
      reifyTransaction (ConT ''Read `AppT` ConT ''Int)
    )


assertElem item list =
  assertBoolVerbose 
    ("Expected item '" ++ item ++ "' to be in " ++ show list)
    (item `elem` list)

