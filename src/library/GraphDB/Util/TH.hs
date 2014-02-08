module GraphDB.Util.TH where

import GraphDB.Util.Prelude
import Language.Haskell.TH



caseLambda :: [Match] -> Exp
caseLambda matches = LamE [VarP argName] (CaseE (VarE argName) matches)
  where
    argName = mkName "_0"

caseFunDec :: Name -> [Match] -> Dec
caseFunDec name matches = 
  FunD name [Clause [VarP argName] (NormalB (CaseE (VarE argName) matches)) []]
  where
    argName = mkName "_0"

