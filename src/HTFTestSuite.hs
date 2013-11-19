{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import GraphDB.Prelude

import {-@ HTF_TESTS @-} HTFTestSuite.GraphTests
import {-@ HTF_TESTS @-} HTFTestSuite.GraphDBTests
import {-@ HTF_TESTS @-} HTFTestSuite.GenerateBoilerplateTests
import {-@ HTF_TESTS @-} HTFTestSuite.GenerateBoilerplate.TransactionReificationTests



main = htfMain $ htf_thisModulesTests : htf_importedTests
