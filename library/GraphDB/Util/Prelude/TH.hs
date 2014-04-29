module GraphDB.Util.Prelude.TH
(
  module Exports,

  purify,
  tryToReify,
  isInstance',
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

isInstance' :: Name -> [Type] -> Q Bool
isInstance' name types = recover (return False) (isInstance name types)

isProperInstance' :: Name -> [Type] -> Q Bool
isProperInstance' name types = recover (return False) (isProperInstance name types)
