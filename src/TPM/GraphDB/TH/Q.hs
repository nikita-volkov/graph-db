-- |
-- General utils for Template Haskell.
module TPM.GraphDB.TH.Q where

import TPM.GraphDB.Prelude
import Language.Haskell.TH




functionArgsAndResult :: Name -> Q (Maybe ([Type], Type))
functionArgsAndResult name = do
  info <- reify name
  case info of
    VarI _ t _ _ -> return $ Just $ typeToArgsAndResult t
    _ -> return Nothing
  where
    typeToArgsAndResult :: Type -> ([Type], Type)
    typeToArgsAndResult t = 
      case t of
        (AppT (AppT ArrowT a) b) -> 
          case typeToArgsAndResult b of 
            (args, result) -> (a : args, result)
        t -> ([], t)

listLocalFunctions :: Q [Name]
listLocalFunctions = do 
  loc <- location
  file <- runIO $ readFile $ loc_filename loc
  return $ map mkName $ nub $ map fst $ concat $ map lex $ lines file

