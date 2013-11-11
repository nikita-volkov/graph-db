{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import TPM.GraphDB.Prelude

import {-@ HTF_TESTS @-} MainTestSuite.GraphTests
import {-@ HTF_TESTS @-} MainTestSuite.GraphDBTests
import {-@ HTF_TESTS @-} MainTestSuite.GenerateBoilerplateTests



main = htfMain $ htf_thisModulesTests : htf_importedTests
