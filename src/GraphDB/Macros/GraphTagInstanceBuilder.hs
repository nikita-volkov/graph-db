module GraphDB.Macros.GraphTagInstanceBuilder where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.API as API
import qualified GraphDB.TH as TH
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.Macros.NamesRegistry as NamesRegistry
import qualified Data.Set as Set

data GraphTagInstanceBuilder = GraphTagInstanceBuilder {
  addValue :: Type -> Q (Name, Name),
  render :: Q Dec
}

new :: (Name, Type) -> Q GraphTagInstanceBuilder
new (tagName, tagType) = do

  (valueNamesRegistry, valueTypeNamesRegistry) <- 
    Q.liftSTM $ (,) <$>
      NamesRegistry.new (nameBase tagName <> "_UnionValue_") <*>
      NamesRegistry.new (nameBase tagName <> "_UnionValueType_")

  valueToTypeAssocsRegistry <- runIO $ newIORef Set.empty

  let
    addValue t = do
      unionValueName <- Q.liftSTM $ NamesRegistry.resolve valueNamesRegistry t
      unionValueTypeName <- Q.liftSTM $ NamesRegistry.resolve valueTypeNamesRegistry t
      runIO $ modifyIORef valueToTypeAssocsRegistry (Set.insert (unionValueName, unionValueTypeName))
      return (unionValueName, unionValueTypeName)

    render = InstanceD <$> pure [] <*> headQ <*> decsQ
      where
        headQ = pure $ AppT (ConT ''API.GraphTag) tagType
        decsQ = 
          sequence
            [ rootDecQ,
              unionValueDecQ, 
              unionValueTypeDecQ,
              unionValueIndexHashesFunDecQ,
              unionValueAnyFunDecQ,
              unionValueTypeFunDecQ,
              unionValueTypeValueFunDecQ ]
          where
            rootDecQ = pure $ TySynInstD ''API.Root [tagType] tagType
            unionValueDecQ =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''API.UnionValue
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs valueNamesRegistry
                  forM pairs $ \(t, n) -> return $ NormalC n [(NotStrict, t)]
                derivations = [''Eq, ''Generic]
            unionValueTypeDecQ =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''API.UnionValueType
                getConstructors = do
                  names <- Q.liftSTM $ NamesRegistry.getNames valueTypeNamesRegistry
                  forM names $ \n -> return $ NormalC n []
                derivations = [''Eq, ''Generic]
            unionValueIndexHashesFunDecQ = FunD <$> pure 'API.unionValueIndexHashes <*> clausesQ
              where
                clausesQ = do
                  sPairs <- Q.liftSTM $ NamesRegistry.getPairs valueTypeNamesRegistry
                  tPairs <- Q.liftSTM $ NamesRegistry.getPairs valueNamesRegistry
                  clauses <- fmap (catMaybes . join) $ forM sPairs $ \(st, sn) -> forM tPairs $ \(tt, tn) -> do
                    isInstance ''API.Reachable [tagType, st, tt] >>= \case
                      False -> return Nothing
                      True -> Just <$> clauseQ (st, sn) (tt, tn)
                  when (null clauses) $ fail "No clauses generated"
                  return clauses
                  where
                    clauseQ (st, sn) (tt, tn) = 
                      Clause <$> patternsQ <*> bodyQ <*> pure []
                      where
                        targetValueN = mkName "z"
                        patternsQ = pure [ConP sn [], ConP tn [VarP targetValueN]]
                        bodyQ = NormalB <$>
                          [e|
                            map hash (API.indexes $(varE targetValueN) :: [API.Index $(return tagType) $(return st) $(return tt)])
                          |]
            unionValueAnyFunDecQ =
              TH.caseFunDec <$> pure 'API.unionValueAny <*> matchesQ
              where
                matchesQ = do
                  names <- Q.liftSTM $ NamesRegistry.getNames valueNamesRegistry
                  when (null names) $ fail "No union values to render"
                  mapM matchQ names
                  where
                    matchQ uvn =
                      Match <$> pure pattern <*> bodyQ <*> pure []
                      where
                        valueVN = mkName "z"
                        pattern = ConP uvn [VarP valueVN]
                        bodyQ = NormalB <$>
                          [e| unsafeCoerce $(varE valueVN) |]
            unionValueTypeFunDecQ =
              TH.caseFunDec <$> pure 'API.unionValueType <*> matchesQ
              where
                matchesQ = do
                  assocs <- Set.toList <$> (runIO $ readIORef valueToTypeAssocsRegistry)
                  when (null assocs) $ fail "No union associations to render"
                  mapM matchQ assocs
                  where
                    matchQ (uvn, uvtn) =
                      Match <$> pure pattern <*> bodyQ <*> pure []
                      where
                        valueVN = mkName "z"
                        pattern = ConP uvn [VarP valueVN]
                        bodyQ = NormalB <$>
                          [e| $(conE uvtn) |]
            unionValueTypeValueFunDecQ = FunD <$> pure 'API.unionValueTypeValue <*> clausesQ
              where
                clausesQ = do
                  assocs <- Set.toList <$> (runIO $ readIORef valueToTypeAssocsRegistry)
                  when (null assocs) $ fail "No union associations to render"
                  mapM clauseQ assocs
                  where
                    clauseQ (uvn, uvtn) =
                      Clause <$> pure [VarP $ mkName "any", ConP uvtn []] <*> bodyQ <*> pure []
                      where
                        bodyQ = NormalB <$> 
                          [e| $(conE uvn) $ unsafeCoerce $(varE $ mkName "any") |]


  return $ 
    GraphTagInstanceBuilder 
      addValue
      render
