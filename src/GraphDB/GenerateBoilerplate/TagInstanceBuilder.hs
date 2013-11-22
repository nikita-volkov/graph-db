module GraphDB.GenerateBoilerplate.TagInstanceBuilder where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.API as API
import qualified GraphDB.TH.Q as Q

data TagInstanceBuilder = TagInstanceBuilder {
  addMemberValueConstructor :: Name -> Type -> CIO (),
  addMemberEdgeConstructor :: Name -> Type -> CIO (),
  addMemberEventConstructor :: Name -> Type -> CIO (),
  addMemberEventResultConstructor :: Name -> Type -> CIO (),
  addMemberEventTransactionClause :: Name -> Name -> CIO (),
  getDec :: CIO Dec
}

new :: Type -> CIO TagInstanceBuilder
new tagType = do
  memberValueConstructorsVar <- liftSTM $ newTVar []
  memberEdgeConstructorsVar <- liftSTM $ newTVar []
  memberEventConstructorsVar <- liftSTM $ newTVar []
  memberEventResultConstructorsVar <- liftSTM $ newTVar []
  memberEventTransactionMatchesVar <- liftSTM $ newTVar []
  let
    addMemberValueConstructor name t =
      liftSTM $ modifyTVar memberValueConstructorsVar (addIfNotElem c)
      where
        c = NormalC name [(IsStrict, t)]
    addMemberEdgeConstructor name t =
      liftSTM $ modifyTVar memberEdgeConstructorsVar (addIfNotElem c)
      where
        c = NormalC name [(IsStrict, t)]
    addMemberEventConstructor name t =
      liftSTM $ modifyTVar memberEventConstructorsVar (addIfNotElem c)
      where
        c = NormalC name [(IsStrict, t)]
    addMemberEventResultConstructor name t =
      liftSTM $ modifyTVar memberEventResultConstructorsVar (addIfNotElem c)
      where
        c = NormalC name [(IsStrict, t)]
    addMemberEventTransactionClause eventName resultName = 
      liftSTM $ modifyTVar memberEventTransactionMatchesVar (match:)
      where
        match = Match pattern body decs
          where
            pattern = ConP eventName [VarP patternVarName]
            patternVarName = mkName "e"
            body = NormalB $ Q.purify $
              [e| $(conE resultName) <$> API.eventTransaction $(varE patternVarName) |]
            decs = []
    addIfNotElem a b = if elem a b then b else a:b
    getDec = InstanceD <$> pure [] <*> getHead <*> getDecs
      where
        getHead = pure $ AppT (ConT ''API.Tag) tagType
        getDecs = 
          sequenceConcurrently
            [ getMemberValueDec, 
              getMemberEdgeDec, 
              getMemberEventDec, 
              getMemberEventResultDec,
              getMemberEventTransactionDec ]
          where
            getMemberValueDec = 
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = mkName "MemberValue"
                getConstructors = liftSTM $ readTVar memberValueConstructorsVar
                derivations = [''Eq, ''Generic]
            getMemberEdgeDec =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = mkName "MemberEdge"
                getConstructors = liftSTM $ readTVar memberEdgeConstructorsVar
                derivations = [''Eq, ''Generic]
            getMemberEventDec =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = mkName "MemberEvent"
                getConstructors = liftSTM $ readTVar memberEventConstructorsVar
                derivations = [''Eq, ''Generic]
            getMemberEventResultDec =
              DataInstD <$> pure [] <*> pure name <*> pure [tagType] <*> getConstructors <*> pure derivations
              where
                name = mkName "MemberEventResult"
                getConstructors = liftSTM $ readTVar memberEventResultConstructorsVar
                derivations = [''Eq, ''Generic]
            getMemberEventTransactionDec = FunD <$> pure name <*> sequence [getClause]
              where
                name = mkName "memberEventTransaction"
                getClause = Clause <$> pure [VarP $ mkName "e"] <*> getBody <*> pure []
                  where
                    getBody = NormalB . CaseE (VarE $ mkName "e") <$> getMatches
                      where
                        getMatches = liftSTM $ readTVar memberEventTransactionMatchesVar

    in return $ 
      TagInstanceBuilder 
        addMemberValueConstructor
        addMemberEdgeConstructor
        addMemberEventConstructor
        addMemberEventResultConstructor
        addMemberEventTransactionClause
        getDec


