module GraphDB.TH.Q where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.TH.Type as Type
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Char as Char



reifyLocalFunctions :: Q [(Name, [Type], Type)]
reifyLocalFunctions =
  listLocalFunctions >>=
  mapM (\name -> reifyFunction name >>= mapM (\(a, b) -> return (name, a, b))) >>=
  return . catMaybes

listLocalFunctions :: Q [Name]
listLocalFunctions = do 
  loc <- location
  text <- runIO $ Text.readFile $ loc_filename loc
  return $ map (mkName . Text.unpack) $ nub $ parse text
  where
    parse text = 
      either (error . ("Local function name parsing failure: " ++)) id $
      AP.parseOnly parser text
      where
        parser = 
          AP.sepBy (optional topLevelFunctionP <* AP.skipWhile (not . AP.isEndOfLine)) 
                   AP.endOfLine >>=
          return . catMaybes
          where
            -- NOTE: Actually, there's no need in precise parsing fuss, 
            -- since we're down to error-handling strategy using 'tryToReify'.
            topLevelFunctionP = do
              name <- valNameP
              many (AP.skipSpace *> unscopedPatternP)
              AP.skipSpace
              AP.char '='
              return name
              where
                valNameP = do
                  head <- AP.satisfy Char.isLower
                  tail <- many (AP.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'']))
                  return $ Text.pack $ head : tail
                unscopedPatternP = 
                  void valNameP <|> litP <|> tupP <|> parensP <|> asP <|> wildP <|> recP <|> listP
                parensP = AP.char '(' *> scopedPatternP <* AP.char ')'
                scopedPatternP = unscopedPatternP <|> conP
                litP = void charP <|> void stringP <|> void AP.number
                  where
                    charP = AP.char '\'' *> AP.anyChar <* AP.char '\''
                    stringP = AP.char '\"' *> AP.takeWhile (/= '\"') <* AP.char '\"'
                tupP = do
                  AP.char '('
                  AP.skipSpace
                  AP.sepBy scopedPatternP sepP
                  AP.skipSpace
                  AP.char ')'
                  return ()
                  where
                    sepP = AP.skipSpace *> AP.char ',' <* AP.skipSpace
                asP = do
                  valNameP
                  AP.skipSpace
                  AP.char '@'
                  AP.skipSpace
                  unscopedPatternP
                wildP = AP.char '_' *> pure ()
                recP = do
                  typeNameP
                  AP.skipSpace
                  AP.char '{'
                  AP.skipSpace
                  AP.sepBy fieldPatP sepP
                  AP.skipSpace
                  AP.char '}'
                  return ()
                  where
                    fieldPatP = nameToPatP <|> wildCardP
                      where
                        nameToPatP = do
                          valNameP
                          AP.skipSpace
                          AP.char '='
                          AP.skipSpace
                          scopedPatternP
                          return ()
                        wildCardP = AP.string ".." *> pure ()
                    sepP = AP.skipSpace *> AP.char ',' <* AP.skipSpace
                typeNameP = do
                  head <- AP.satisfy Char.isUpper
                  tail <- many (AP.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'']))
                  return $ Text.pack $ head : tail
                listP = do
                  AP.char '['
                  AP.skipSpace
                  AP.sepBy scopedPatternP sepP
                  AP.skipSpace
                  AP.char ']'
                  return ()
                  where
                    sepP = AP.skipSpace *> AP.char ',' <* AP.skipSpace
                conP = typeNameP *> many (AP.skipSpace *> unscopedPatternP) *> pure ()

reifyFunction :: Name -> Q (Maybe ([Type], Type))
reifyFunction name = do
  info <- tryToReify name
  case info of
    Just (VarI _ t _ _) -> return $ Just $ Type.argsAndResult $ Type.unforall t
    _ -> return Nothing

reifyType :: Name -> Q (Maybe Type)
reifyType name = do
  info <- tryToReify name
  case info of
    Just (TyConI _) -> Just <$> conT name
    _ -> return Nothing

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 

purify :: Q a -> a
purify = unsafePerformIO . runQ

run :: MonadIO m => Q a -> m a
run = liftIO . runQ

caseLambda :: [Q Match] -> Q Exp
caseLambda matches = lamE [varP argName] (caseE (varE argName) matches)
  where
    argName = mkName "_0"


-- |
-- Only checks the instances in scope of the calling site,
-- it will not detect the declared instances, if they are not imported.
whenNoInstance :: Monoid a => Name -> [Type] -> Q a -> Q a
whenNoInstance name types f = do
  z <- recover (return False) (isInstance name types)
  if z
    then return mempty
    else f
