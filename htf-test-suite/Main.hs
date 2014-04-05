{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import GraphDB.Util.Prelude

import {-@ HTF_TESTS @-} HTFTestSuite.GraphDBTests
import {-@ HTF_TESTS @-} HTFTestSuite.Macros.TransactionReificationTests
import {-@ HTF_TESTS @-} HTFTestSuite.StorageTests


main = htfMain $ htf_thisModulesTests : htf_importedTests
