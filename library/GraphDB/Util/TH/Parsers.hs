module GraphDB.Util.TH.Parsers where

import GraphDB.Util.Prelude
import qualified Language.Haskell.TH as T
import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified GraphDB.Util.TH.Type as Type


type Parse a = (P.ParsecT String () T.Q a, Name)
type Name = String

runParse :: Parse r -> T.Q r
runParse (p, n) = do
  loc <- T.location
  content <- T.runIO $ readFile $ T.loc_filename loc
  P.runParserT p () n content
    >>= either (fail . ("Parse failed: " <>) . show) return


type Instance = (T.Name, [T.Type])

-- | 
-- N.B.: works only on instances with a /where/ clause and concrete parameters.
instances :: Parse [Instance]
instances = (p, "Instances")
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
      P.try con <|> P.try tuple <|> P.try list <|> P.try (inBraces con) <|> inBraces getParamType
      where
        con = getTypeName |> fmap T.ConT
        tuple = Type.tuple <$> itemsP
          where
            itemsP = 
              P.char '(' *> optional skipSpace *>
              P.sepBy getParamType (optional skipSpace *> P.char ',' <* optional skipSpace) <*
              optional skipSpace <* P.char ')'
        list = 
          T.AppT T.ListT <$>
          (P.char '[' *> optional skipSpace *> getParamType <* optional skipSpace <* P.char ']')
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
        Nothing -> error $ "Type not found: " <> identifier
    getUpperIdentifier = do
      head <- P.satisfy Char.isUpper
      tail <- many (P.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'']))
      return $ head : tail
