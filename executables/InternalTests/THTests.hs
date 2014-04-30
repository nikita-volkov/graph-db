{-# OPTIONS_GHC -F -pgmF htfpp #-}
module InternalTests.THTests where

import Test.Framework
import GraphDB.Util.Prelude
import Language.Haskell.TH
import qualified GraphDB.Util.TH as T
import qualified GraphDB.Util.TH.Parsers as P
import qualified GraphDB.Model as M

data A a b
data B

test_parseMultiparamInstance :: IO ()
test_parseMultiparamInstance = do
  assertEqual 
    "[(GraphDB.Model.Edge,[ConT InternalTests.THTests.B,ConT InternalTests.THTests.B])]"
    $(
      let content = "instance M.Edge B B where"
          in stringE . show =<< P.runParse content P.instances
    )

test_parseInstanceWithPolymorphicTypes :: IO ()
test_parseInstanceWithPolymorphicTypes = do
  assertEqual 
    "[(GraphDB.Model.Edge,[AppT (AppT (ConT InternalTests.THTests.A) (ConT InternalTests.THTests.B)) (ConT InternalTests.THTests.B),ConT InternalTests.THTests.B])]"
    $(
      let content = "instance M.Edge (A B B) B where"
          in stringE . show =<< P.runParse content P.instances
    )

test_parseInstanceWithVariables :: IO ()
test_parseInstanceWithVariables = do
  assertEqual 
    "[(GraphDB.Model.Edge,[AppT (AppT (ConT InternalTests.THTests.A) (ConT InternalTests.THTests.B)) (VarT a),VarT b])]"
    $(
      let content = "instance M.Edge (A B a) b where"
          in stringE . show =<< P.runParse content P.instances
    )
