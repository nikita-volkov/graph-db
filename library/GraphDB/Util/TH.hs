module GraphDB.Util.TH where

import GraphDB.Util.Prelude
import Language.Haskell.TH
import qualified GraphDB.Util.TH.Parsers as P



caseLambda :: [Match] -> Exp
caseLambda matches = LamE [VarP argName] (CaseE (VarE argName) matches)
  where
    argName = mkName "_0"

caseFunDec :: Name -> [Match] -> Dec
caseFunDec name matches = 
  FunD name [Clause [VarP argName] (NormalB (CaseE (VarE argName) matches)) []]
  where
    argName = mkName "_0"

reifyLocalInstances :: Q [(Name, [Type])]
reifyLocalInstances = do
  loc <- location
  text <- runIO $ readFile $ loc_filename loc
  P.runParse text P.instances
