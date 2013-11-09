module TPM.GraphDB.TH.Type where

import TPM.GraphDB.Prelude
import Language.Haskell.TH

argsAndResult :: Type -> ([Type], Type)
argsAndResult t = case t of
  AppT (AppT ArrowT a) b -> case argsAndResult b of (args, result) -> (a : args, result)
  t -> ([], t)

-- |
-- Uncurry a type into a list of args.
-- 
-- >>> runQ [t|[Int] -> Int -> Char|] >>= return . unarrow
-- [AppT ListT (ConT GHC.Types.Int),ConT GHC.Types.Int,ConT GHC.Types.Char]
-- 
unarrow :: Type -> [Type]
unarrow t = case t of
  AppT (AppT ArrowT a) b -> a : unarrow b
  _ -> [t]

-- |
-- Explode a type into a list of applications in reverse order.
-- 
-- >>> data A a b c
-- >>> runQ [t|A Int Char Bool|] >>= return . unapply
-- [ConT GHC.Types.Bool,ConT GHC.Types.Char,ConT GHC.Types.Int,ConT :Interactive.A]
-- 
unapply :: Type -> [Type]
unapply t = case t of
  AppT a b -> b : unapply a
  _ -> [t]

unforall :: Type -> Type
unforall t = case t of
  ForallT _ _ t' -> t'
  _ -> t
