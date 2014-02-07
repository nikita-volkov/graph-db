module GraphDB.Macros where

import GraphDB.Util.Prelude
import Language.Haskell.TH
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Text.Parsec as P
import qualified GraphDB.Engine as Engine
import qualified GraphDB.Macros.NamesRegistry as NamesRegistry; import GraphDB.Macros.NamesRegistry (NamesRegistry)
import qualified GraphDB.Util.TH as TH
import qualified GraphDB.Util.TH.Q as Q
import qualified GraphDB.Util.TH.Type as Type
import qualified GraphDB.Macros.BoilerplateBuilder as BoilerplateBuilder


-- |
-- Scans the current module for all transaction-functions and 
-- generates appropriate \"event\" data-types, 
-- associating them with the provided database tag.
-- Also scans for all instances of 'GraphDB.Engine.Edge' to identify
-- the supported node-value types.
-- 
-- All transaction-functions and 'GraphDB.Engine.Edge' declarations must be located 
-- in the same module where this macro gets called.
-- 
generateBoilerplate :: Name -> Q [Dec]
generateBoilerplate tagName = do

  tagType <-
    Q.reifyType tagName >>= 
    maybe (fail $ "Not a type name: " ++ show tagName) return

  BoilerplateBuilder.BoilerplateBuilder addEdgePair addTransactionFunction getDecs <- 
    BoilerplateBuilder.new (tagName, tagType)

  do
    reachablePairs <- reifyLocalEdgePairs
    when (null reachablePairs) $ fail "No 'Edge' instances found in module"
    forM_ reachablePairs $ addEdgePair

  do
    reifiedFunctions <- reifyLocalTransactionFunctions tagType
    when (null reifiedFunctions) $ fail "No transaction-functions found in module"
    traverse_ addTransactionFunction reifiedFunctions

  getDecs


reifyLocalTransactionFunctions :: Type -> Q [(Name, [Type], Type, Bool)]
reifyLocalTransactionFunctions tagType = 
  Q.reifyLocalFunctions >>= fmap catMaybes . traverse processTransactionFunction
  where
    processTransactionFunction (name, t) =
      sequence [process1, process2] >>= return . msum
      where
        process1 = 
          processTransactionFunctionResult t >>= 
          return . join . fmap (fromTransactionType [])
        process2 = do
          if all containsNoVarT args
            then 
              processTransactionFunctionResult result >>= 
              return . join . fmap (fromTransactionType args)
            else return Nothing
          where
            (args, result) = Type.argsAndResult $ Type.unforall t
            containsNoVarT = \case
              ForallT _ _ t -> containsNoVarT t
              AppT a b -> containsNoVarT a && containsNoVarT b
              VarT{} -> False
              _ -> True
        fromTransactionType args (isWrite, trTagType, trResultType) = do
          assertZ $ trTagType == tagType
          return (name, args, trResultType, isWrite)

processTransactionFunctionResult :: Type -> Q (Maybe (Bool, Type, Type))
processTransactionFunctionResult = \case
  trType `AppT` tagType `AppT` _ `AppT` resultType 
    | isNode -> return Nothing
    | isTransaction -> return $ Just (trType == writeType, tagType, resultType)
    where
      isTransaction = trType `elem` [writeType, readType]
      isNode = case Type.unapply resultType of
        _ : _ : _ : z : [] | z == nodeRefType -> True
        _ -> False
        where
          nodeRefType = Q.purify [t| Engine.Node |]
      writeType = Q.purify [t| Engine.Write |]
      readType = Q.purify [t| Engine.Read |]
  -- A very special case for `ReadOrWrite` type-synonym:
  ForallT 
    [_, KindedTV tVarName _] 
    constraints 
    (AppT
      (AppT
        (AppT
          (AppT (VarT tVarName') (AppT memberValueType tagType))
          tagType')
        _)
      resultType)
    | memberValueType == ConT ''Engine.UnionValue,
      tVarName == tVarName',
      tagType == tagType',
      any ((== Just tVarName) . transactionConstraintVarName) constraints ->
    return $ Just (False, tagType, resultType)
    where
      transactionConstraintVarName = \case
        ClassP className [VarT varName] | className == ''Engine.Transaction -> Just varName
        _ -> Nothing
  t@ForallT{} -> processTransactionFunctionResult $ Type.unforall t
  t -> Q.expandRootSynType t >>= fmap join . traverse processTransactionFunctionResult

-- TODO: probably, analyze the import statements to get the qualified
-- alias for the class name.
-- On the other hand, the user may be reexporting it from unknown module.
reifyLocalEdgePairs :: Q [(Type, Type)]
reifyLocalEdgePairs = do
  loc <- location
  text <- runIO $ readFile $ loc_filename loc
  P.runParserT getInstances () "'Edge' instances" text
    >>= either (error . ("Parser failed: " <>) . show) return
  where
    getInstances = 
      catMaybes <$>
      P.sepBy (optional (instanceP) <* P.skipMany (P.noneOf "\n\r")) skipEOL
    skipEOL = P.skipMany1 (P.oneOf "\n\r")
    skipSpace = P.skipMany1 P.space
    instanceP = P.try $ do
      P.string "instance"
      skipSpace
      optional $ skipConstraints *> skipSpace
      className <- classNameP
      when (className /= "Edge") $ fail "Not a 'Edge' instance"
      skipSpace
      source <- paramTypeP
      skipSpace
      target <- paramTypeP
      skipSpace
      P.string "where"
      return (source, target)
    paramTypeP = 
      conP <|> tupleP <|> listP <|> inBracesP conP <|> inBracesP paramTypeP
      where
        conP = do
          identifier <- upperIdentifierP
          lift (lookupTypeName identifier) >>= \case
            Nothing -> fail $ "Type not found: " <> identifier
            Just n -> return $ ConT n
        tupleP = Type.tuple <$> itemsP
          where
            itemsP = P.try $
              P.char '(' *> optional skipSpace *>
              P.sepBy paramTypeP (optional skipSpace *> P.char ',' <* optional skipSpace) <*
              optional skipSpace <* P.char ')'
        listP = P.try $
          AppT ListT <$>
          (P.char '[' *> optional skipSpace *> paramTypeP <* optional skipSpace <* P.char ']')
    inBracesP p = P.try $ P.char '(' *> p <* P.char ')'
    skipConstraints =
      P.try $ 
        (skipConstraintClause <|> skipAnythingInBraces) *> 
        P.skipMany P.space *>
        P.string "=>" *> pure ()
      where
        skipAnythingInBraces =
          P.try $ P.char '(' *> P.manyTill skip (P.char ')') *> pure ()
          where
            skip = skipAnythingInBraces <|> P.skipMany (P.noneOf "()")
        skipConstraintClause = 
          P.try $ void $ P.sepEndBy1 (void paramTypeP <|> skipAnythingInBraces) P.spaces
    classNameP = 
      (P.try $ upperIdentifierP *> P.char '.' *> upperIdentifierP) <|>
      upperIdentifierP
    upperIdentifierP = P.try $ do
      head <- P.satisfy Char.isUpper
      tail <- many (P.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'']))
      return $ head : tail

