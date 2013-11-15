{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import TPM.GraphDB.Prelude

import {-@ HTF_TESTS @-} HTFTestSuite.GraphTests
import {-@ HTF_TESTS @-} HTFTestSuite.GraphDBTests
import {-@ HTF_TESTS @-} HTFTestSuite.GenerateBoilerplateTests



main = htfMain $ htf_thisModulesTests : htf_importedTests
