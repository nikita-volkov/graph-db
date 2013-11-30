module GraphDB.Macros.GraphTagInstanceBuilder where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.API as API
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.Macros.NamesRegistry as NamesRegistry

data GraphTagInstanceBuilder = GraphTagInstanceBuilder {
  addValue :: Type -> Q (Name, Name),
  getDec :: Q Dec
}

new :: (Name, Type) -> Q GraphTagInstanceBuilder
new (tagName, tagType) = do

  (valueNamesRegistry, valueTypeNamesRegistry) <- 
    Q.liftSTM $ (,) <$>
      NamesRegistry.new (nameBase tagName <> "_UnionValue_") <*>
      NamesRegistry.new (nameBase tagName <> "_UnionValueType_")

  let
    addValue t = do
      unionValueName <- Q.liftSTM $ NamesRegistry.resolve valueNamesRegistry t
      unionValueTypeName <- Q.liftSTM $ NamesRegistry.resolve valueTypeNamesRegistry t
      return (unionValueName, unionValueTypeName)

    getDec = InstanceD <$> pure [] <*> getHead <*> getDecs
      where
        getHead = pure $ AppT (ConT ''API.GraphTag) tagType
        getDecs = 
          sequence
            [ getRootDec,
              getUnionValueDec, 
              getUnionValueTypeDec,
              getUnionIndexHashesDec ]
          where
            getRootDec = pure $ TySynInstD ''API.Root [tagType] tagType
            getUnionValueDec =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''API.UnionValue
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs valueNamesRegistry
                  forM pairs $ \(t, n) -> return $ NormalC n [(IsStrict, t)]
                derivations = [''Eq, ''Generic]
            getUnionValueTypeDec =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''API.UnionValueType
                getConstructors = do
                  names <- Q.liftSTM $ NamesRegistry.getNames valueTypeNamesRegistry
                  forM names $ \n -> return $ NormalC n []
                derivations = [''Eq, ''Generic]
            getUnionIndexHashesDec = FunD <$> pure name <*> sequence [getClause]
              where
                name = 'API.unionIndexHashes
                getClause = Clause <$> pure [VarP $ mkName "a"] <*> getBody <*> pure []
                  where
                    getBody = NormalB . CaseE (VarE $ mkName "a") <$> getMatches
                      where
                        getMatches = do
                          sPairs <- Q.liftSTM $ NamesRegistry.getPairs valueTypeNamesRegistry
                          tPairs <- Q.liftSTM $ NamesRegistry.getPairs valueNamesRegistry
                          found <- fmap (catMaybes . join) $ forM sPairs $ \(st, sn) -> forM tPairs $ \(tt, tn) -> do
                            isInstance ''API.Reachable [tagType, st, tt] >>= \case
                              False -> return Nothing
                              True -> Just <$> getMatch (st, sn) (tt, tn)
                          wild <- wildMatchQ
                          return $ found ++ [wild]
                          where
                            getMatch (st, sn) (tt, tn) = Match <$> patternQ <*> bodyQ <*> pure []
                              where
                                targetValueN = mkName "z"
                                patternQ = pure $ TupP [ConP tn [VarP targetValueN], ConP sn []]
                                bodyQ = NormalB <$>
                                  [e|
                                    let
                                      ixs :: [API.Index $(return tagType) $(return st) $(return tt)]
                                      ixs = API.indexes $(varE targetValueN)
                                      in map hash ixs
                                  |]
                            wildMatchQ = Match <$> pure WildP <*> bodyQ <*> pure []
                              where
                                bodyQ = NormalB <$> 
                                  [e| error "There's no 'Reachable' instance for provided types" |]

  return $ 
    GraphTagInstanceBuilder 
      addValue
      getDec
