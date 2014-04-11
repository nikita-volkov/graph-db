module GraphDB.Util.TH.Parsers where

import GraphDB.Util.Prelude
import qualified Language.Haskell.TH as T
import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified GraphDB.Util.TH.Type as Type


type Parse = P.ParsecT String ()

runParse :: (Monad m) => String -> Parse m r -> m r
runParse content p = do
  P.runParserT p () "runParse parser" content
    >>= either (fail . ("Parse failed: " <>) . show) return

type Instance = (T.Name, [T.Type])

-- | 
-- N.B.: works only on instances with a /where/ clause and concrete parameters.
instances :: Parse T.Q [Instance]
instances = p
  where
    p = P.sepBy (optional (P.try getInstance) <* P.skipMany (P.noneOf "\n\r")) skipEOL |> 
        fmap catMaybes
    skipEOL = P.skipMany1 (P.oneOf "\n\r")
    skipSpace = P.skipMany1 P.space
    getInstance = do
      P.string "instance"
      skipSpace
      optional $ P.try skipConstraints *> skipSpace
      className <- getTypeName
      skipSpace
      params <- P.sepEndBy1 (getParamType) skipSpace
      P.string "where"
      return (className, params)
    getParamType = 
      P.try (inBraces app) <|>
      P.try nonApp <|>
      inBraces getParamType
      where
        nonApp = 
          con <|>
          P.try var <|>
          P.try tuple <|>
          P.try list <|>
          inBraces nonApp
        var = T.VarT . T.mkName <$> getLowerIdentifier
        con = T.ConT <$> getTypeName
        tuple = Type.tuple <$> itemsP
          where
            itemsP = 
              P.char '(' *> optional skipSpace *>
              P.sepBy getParamType (optional skipSpace *> P.char ',' <* optional skipSpace) <*
              optional skipSpace <* P.char ')'
        list = 
          T.AppT T.ListT <$>
          (P.char '[' *> optional skipSpace *> getParamType <* optional skipSpace <* P.char ']')
        app = do
          a <- getParamType
          skipSpace
          b <- P.try app <|> getParamType
          return $ case b of
            T.AppT b1 b2 -> T.AppT (T.AppT a b1) b2
            _ -> T.AppT a b
    inBraces p = P.char '(' *> p <* P.char ')'
    skipConstraints =
      (P.try skipConstraintClause <|> skipAnythingInBraces) *> 
      P.skipMany P.space *>
      P.string "=>" *> pure ()
      where
        skipAnythingInBraces = 
          P.char '(' *> P.manyTill skip (P.char ')') *> pure ()
          where
            skip = skipAnythingInBraces <|> P.skipMany (P.noneOf "()")
        skipConstraintClause = 
          void $ P.sepEndBy1 (void getParamType <|> skipAnythingInBraces) P.spaces
    getTypeName = do
      identifier <- P.sepBy1 getUpperIdentifier (P.char '.') |> fmap (intercalate ".")
      T.lookupTypeName identifier |> lift >>= \case
        Just n -> return n
        Nothing -> lift $ fail $ "Type not found: " <> identifier
    getUpperIdentifier = do
      head <- P.satisfy Char.isUpper
      tail <- many (P.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'']))
      return $ head : tail
    getLowerIdentifier = do
      head <- P.satisfy Char.isLower
      tail <- many (P.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'']))
      let i = head : tail
      if elem i ["where"]
        then empty
        else return i
