module GraphDB.Macros.GraphDBTagInstanceBuilder where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.API as API
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.Macros.NamesRegistry as NamesRegistry

data GraphDBTagInstanceBuilder = GraphDBTagInstanceBuilder {
  addEventAndEventResult :: (Type, Type) -> Q (Name, Name),
  getDec :: Q Dec
}

new :: (Name, Type) -> Q GraphDBTagInstanceBuilder
new (tagName, tagType) = do

  (eventsMR, eventResultsMR) <- 
    Q.liftSTM $ (,) <$>
      NamesRegistry.new (nameBase tagName <> "_UnionEvent_") <*>
      NamesRegistry.new (nameBase tagName <> "_UnionEventResult_")

  unionEventTransactionMatchesRef <- runIO $ newIORef []

  let
    addEventAndEventResult (eventT, eventResultT) = do
      unionEventName <- Q.liftSTM $ NamesRegistry.resolve eventsMR eventT
      unionEventResultName <- Q.liftSTM $ NamesRegistry.resolve eventResultsMR eventResultT
      addUnionEventTransactionMatch unionEventName unionEventResultName
      return (unionEventName, unionEventResultName)
    addUnionEventTransactionMatch eventName resultName = 
      runIO $ modifyIORef unionEventTransactionMatchesRef (match:)
      where
        match = Match pattern body decs
          where
            pattern = ConP eventName [VarP patternVarName]
            patternVarName = mkName "e"
            body = NormalB $ Q.purify $
              [e| $(conE resultName) <$> API.eventTransaction $(varE patternVarName) |]
            decs = []
    getDec = InstanceD <$> pure [] <*> getHead <*> getDecs
      where
        getHead = pure $ AppT (ConT ''API.GraphDBTag) tagType
        getDecs = 
          sequence
            [ getUnionEventDec, 
              getUnionEventResultDec,
              getUnionEventTransactionDec ]
          where
            getUnionEventDec =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''API.UnionEvent
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs eventsMR
                  forM pairs $ \(t, n) -> return $ NormalC n [(IsStrict, t)]
                derivations = [''Eq, ''Generic]
            getUnionEventResultDec =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = ''API.UnionEventResult
                getConstructors = do
                  pairs <- Q.liftSTM $ NamesRegistry.getPairs eventResultsMR
                  forM pairs $ \(t, n) -> return $ NormalC n [(IsStrict, t)]
                derivations = [''Eq, ''Generic]
            getUnionEventTransactionDec = FunD <$> pure name <*> sequence [getClause]
              where
                name = 'API.unionEventTransaction
                getClause = Clause <$> pure [VarP $ mkName "e"] <*> getBody <*> pure []
                  where
                    getBody = NormalB . CaseE (VarE $ mkName "e") <$> getMatches
                      where
                        getMatches = runIO $ readIORef unionEventTransactionMatchesRef

  return $ 
    GraphDBTagInstanceBuilder 
      addEventAndEventResult
      getDec


