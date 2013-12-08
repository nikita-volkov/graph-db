module GraphDB.Macros.TagInstanceBuilder where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.Engine as Engine
import qualified GraphDB.TH as TH
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.Macros.NamesRegistry as NamesRegistry
import qualified Data.Set as Set

data TagInstanceBuilder = TagInstanceBuilder {
  addIndex :: Type -> Q (Name),
  addValue :: Type -> Q (Name, Name),
  addEventAndEventResult :: (Type, Type) -> Q (Name, Name),
  render :: Q Dec
}

new :: (Name, Type) -> Q TagInstanceBuilder
new (tagName, tagType) = do

  (indexNR, valueNR, typeNR, eventNR, eventResultNR) <- 
    Q.liftSTM $ (,,,,) <$>
      NamesRegistry.new (nameBase tagName <> "_UnionIndex_") <*>
      NamesRegistry.new (nameBase tagName <> "_UnionValue_") <*>
      NamesRegistry.new (nameBase tagName <> "_UnionType_") <*>
      NamesRegistry.new (nameBase tagName <> "_UnionEvent_") <*>
      NamesRegistry.new (nameBase tagName <> "_UnionEventResult_")

  unionIndexTargetTypeClauses <- runIO $ newIORef []
  decomposeUnionValueClauses <- runIO $ newIORef []
  composeUnionValueClauses <- runIO $ newIORef []
  unionEventTransactionMatchesRef <- runIO $ newIORef []


  let
    addIndex t = do
      unionName <- Q.liftSTM $ NamesRegistry.resolve indexNR t
      targetUnionTypeName <- Q.liftSTM $ NamesRegistry.resolve typeNR targetType
      let
        clause = Clause [ConP unionName [WildP]] (NormalB body) []
          where
            body = ConE targetUnionTypeName
      runIO $ modifyIORef unionIndexTargetTypeClauses $ (:) clause
      return unionName
      where
        targetType = case t of
          _ `AppT` _ `AppT` z -> z
          _ -> error "Unexpected type of index"
    addValue t = do
      unionValueName <- Q.liftSTM $ NamesRegistry.resolve valueNR t
      unionValueTypeName <- Q.liftSTM $ NamesRegistry.resolve typeNR t
      let
        decomposeUnionValueClause = 
          Clause [ConP unionValueName [VarP varName]] (NormalB exp) []
          where
            varName = mkName "a"
            exp = Q.purify [e| ($(conE unionValueTypeName), unsafeCoerce $(varE varName)) |]
        composeUnionValueClause = 
          Clause [ConP unionValueTypeName [], VarP varName] (NormalB exp) []
          where
            varName = mkName "a"
            exp = Q.purify [e| $(conE unionValueName) $ unsafeCoerce $(varE varName) |]
      runIO $ modifyIORef decomposeUnionValueClauses $ (:) decomposeUnionValueClause
      runIO $ modifyIORef composeUnionValueClauses $ (:) composeUnionValueClause
      return (unionValueName, unionValueTypeName)
    addEventAndEventResult (eventT, eventResultT) = do
      unionEventName <- Q.liftSTM $ NamesRegistry.resolve eventNR eventT
      unionEventResultName <- Q.liftSTM $ NamesRegistry.resolve eventResultNR eventResultT
      addUnionEventTransactionMatch unionEventName unionEventResultName
      return (unionEventName, unionEventResultName)
      where
        addUnionEventTransactionMatch eventName resultName = 
          runIO $ modifyIORef unionEventTransactionMatchesRef (match:)
          where
            match = Match pattern body decs
              where
                pattern = ConP eventName [VarP patternVarName]
                patternVarName = mkName "e"
                body = NormalB $ Q.purify $
                  [e| $(conE resultName) <$> Engine.eventFinalTransaction $(varE patternVarName) |]
                decs = []
    render = InstanceD <$> pure [] <*> headQ <*> decsQ
      where
        headQ = pure $ AppT (ConT ''Engine.Tag) tagType
        decsQ = 
          sequence
            [ rootDecQ,
              unionIndexDecQ, 
              unionValueDecQ, 
              unionTypeDecQ,
              unionEventDecQ, 
              unionEventResultDecQ,
              -- functions
              unionIndexesFunDecQ,
              unionIndexTargetTypeFunDecQ,
              decomposeUnionValueFunDecQ,
              composeUnionValueFunDecQ,
              unionEventTransactionFunDecQ ]
          where
            rootDecQ = pure $ TySynInstD ''Engine.Root [tagType] tagType
            unionIndexDecQ =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''Engine.UnionIndex
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs indexNR
                  forM pairs $ \(t, n) -> return $ NormalC n [(IsStrict, t)]
                derivations = [''Eq, ''Generic]
            unionValueDecQ =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''Engine.UnionValue
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs valueNR
                  forM pairs $ \(t, n) -> return $ NormalC n [(NotStrict, t)]
                derivations = [''Eq, ''Generic]
            unionTypeDecQ =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''Engine.UnionType
                getConstructors = do
                  names <- Q.liftSTM $ NamesRegistry.getNames typeNR
                  forM names $ \n -> return $ NormalC n []
                derivations = [''Eq, ''Generic, ''Show, ''Bounded, ''Enum]
            unionEventDecQ =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''Engine.UnionEvent
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs eventNR
                  forM pairs $ \(t, n) -> return $ NormalC n [(IsStrict, t)]
                derivations = [''Eq, ''Generic]
            unionEventResultDecQ =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''Engine.UnionEventResult
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs eventResultNR
                  forM pairs $ \(t, n) -> return $ NormalC n [(IsStrict, t)]
                derivations = [''Eq, ''Generic]
            unionIndexesFunDecQ = FunD <$> pure 'Engine.unionIndexes <*> clausesQ
              where
                clausesQ = do
                  sPairs <- Q.liftSTM $ NamesRegistry.getPairs typeNR
                  tPairs <- Q.liftSTM $ NamesRegistry.getPairs valueNR
                  clauses <- fmap (catMaybes . join) $ forM sPairs $ \(st, sn) -> forM tPairs $ \(tt, tn) -> do
                    isInstance ''Engine.Edge [tagType, st, tt] >>= \case
                      False -> return Nothing
                      True -> Just <$> clauseQ (st, sn) (tt, tn)
                  when (null clauses) $ fail "No clauses generated"
                  return clauses
                  where
                    clauseQ (st, sn) (tt, tn) = 
                      Clause <$> patternsQ <*> bodyQ <*> pure []
                      where
                        targetValueN = mkName "z"
                        patternsQ = pure [ConP tn [VarP targetValueN], ConP sn []]
                        bodyQ = NormalB <$>
                          [e|
                            map Engine.packIndex (Engine.indexes $(varE targetValueN) :: [Engine.Edge_Index $(return tagType) $(return st) $(return tt)])
                          |]
            unionIndexTargetTypeFunDecQ = do
              clauses <- runIO $ readIORef unionIndexTargetTypeClauses
              when (null clauses) $ fail "No 'unionIndexTargetType' clauses to render"
              return $ FunD 'Engine.unionIndexTargetType clauses
            decomposeUnionValueFunDecQ = do
              clauses <- runIO $ readIORef decomposeUnionValueClauses
              when (null clauses) $ fail "No 'decomposeUnionValue' clauses to render"
              return $ FunD 'Engine.decomposeUnionValue clauses
            composeUnionValueFunDecQ = do
              clauses <- runIO $ readIORef composeUnionValueClauses
              when (null clauses) $ fail "No 'composeUnionValue' clauses to render"
              return $ FunD 'Engine.composeUnionValue clauses
            unionEventTransactionFunDecQ = FunD <$> pure name <*> sequence [clauseQ]
              where
                name = 'Engine.unionEventFinalTransaction
                clauseQ = Clause <$> pure [VarP $ mkName "e"] <*> bodyQ <*> pure []
                  where
                    bodyQ = NormalB . CaseE (VarE $ mkName "e") <$> matchesQ
                      where
                        matchesQ = runIO $ readIORef unionEventTransactionMatchesRef


  return $ 
    TagInstanceBuilder 
      addIndex
      addValue
      addEventAndEventResult
      render
