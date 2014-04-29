module GraphDB.Util.Prelude.TH
(
  module Exports,

  purify,
  tryToReify,
  isProperInstance',
)
where

import GraphDB.Util.Prelude hiding (Fixity)
import Language.Haskell.TH as Exports
import Language.Haskell.TH.Syntax as Exports
import THInstanceReification as Exports


purify :: Q a -> a
purify = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 

isProperInstance' :: Name -> [Type] -> Q Bool
isProperInstance' name types = recover (return False) (isProperInstance name types)
