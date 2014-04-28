{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import GraphDB.Util.Prelude

import {-@ HTF_TESTS @-} InternalTests.StorageTests
import {-@ HTF_TESTS @-} InternalTests.GraphTests
import {-@ HTF_TESTS @-} InternalTests.THTests


main = htfMain $ htf_thisModulesTests : htf_importedTests
