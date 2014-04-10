{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import APITests.Prelude
import APITests.Catalogue
import qualified GraphDB as G

main = htfMain $ htf_thisModulesTests
