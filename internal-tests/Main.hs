{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import GraphDB.Util.Prelude

import {-@ HTF_TESTS @-} InternalTests.StorageTests


main = htfMain $ htf_thisModulesTests : htf_importedTests
