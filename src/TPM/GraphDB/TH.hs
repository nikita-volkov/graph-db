module TPM.GraphDB.TH where

import TPM.GraphDB.Prelude
import qualified TPM.GraphDB.API as API
import Language.Haskell.TH
import qualified TPM.GraphDB.TH.MembersRegistry as MembersRegistry; import TPM.GraphDB.TH.MembersRegistry (MembersRegistry)
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.SafeCopy as SafeCopy
import qualified TPM.GraphDB.TH.AccumulateDecs as AccumulateDecs; import TPM.GraphDB.TH.AccumulateDecs (AccumulateDecs)
import qualified TPM.GraphDB.TH.Q as Q

-- |
-- Scan the current module for all transaction-functions and 
-- generate appropriate \"event\" data-types, 
-- associating them with the provided database tag.
-- 
processTag :: Name -> Q [Dec]
processTag tagName = AccumulateDecs.exec $ do
  tagType <- do
    tagInfo <- AccumulateDecs.liftQ $ reify tagName
    case tagInfo of
      TyConI{} -> return ()
      _ -> error $ "Not a type name: " ++ show tagName
    AccumulateDecs.liftQ $ conT tagName
  
  memberValuesRegistry <- 
    liftIO $ atomically $ MembersRegistry.new (nameBase tagName ++ "MemberValue")
  
  memberEdgesRegistry <- 
    liftIO $ atomically $ MembersRegistry.new (nameBase tagName ++ "MemberEdge")
  
  memberEventsRegistry <- 
    liftIO $ atomically $ MembersRegistry.new (nameBase tagName ++ "MemberEvent")
  
  memberEventResultsRegistry <- 
    liftIO $ atomically $ MembersRegistry.new (nameBase tagName ++ "MemberEventResult")

  memberEventToMemberEventResultPairs <-
    liftIO $ atomically $ newTVar Set.empty

  -- Process functions:
  do
    localFunctionNames <- AccumulateDecs.liftQ Q.listLocalFunctions

    writeType <- AccumulateDecs.liftQ [t| API.Write |]
    readType <- AccumulateDecs.liftQ [t| API.Read |]
    AccumulateDecs.sequenceConcurrently_ $ flip map localFunctionNames $ \functionName -> do
      argsAndResultM <- AccumulateDecs.liftQ $ Q.functionArgsAndResult functionName
      case argsAndResultM of
        Just (argTypes, AppT (AppT (AppT transactionType transactionTagType) _) resultType)
          | transactionType `elem` [writeType, readType] &&
            transactionTagType == tagType -> do
              (eventType, eventCons) <- generateEvent functionName argTypes
              memberEventCons <- liftIO $ atomically $ 
                MembersRegistry.resolve memberEventsRegistry eventType
              generateEventInstance eventType tagType eventCons functionName argTypes resultType (transactionType == writeType)
              generateIsMemberEventOfInstance eventType tagType memberEventCons
              memberEventResultCons <- liftIO $ atomically $ 
                MembersRegistry.resolve memberEventResultsRegistry resultType
              generateIsMemberEventResultOfInstance resultType tagType memberEventResultCons
              forM_ argTypes $ \argType -> do
                if isEdge argType
                  then do
                    memberEdgeCons <- liftIO $ atomically $ 
                      MembersRegistry.resolve memberEdgesRegistry argType
                    generateIsMemberEdgeOfInstance argType tagType memberEdgeCons
                  else do
                    memberValueCons <- liftIO $ atomically $
                      MembersRegistry.resolve memberValuesRegistry argType
                    generateIsMemberValueOfInstance argType tagType memberValueCons

              liftIO $ atomically $ modifyTVar memberEventToMemberEventResultPairs $ 
                Set.insert (memberEventCons, memberEventResultCons)
          where
            resultTypeName = case resultType of ConT n -> n
        _ -> return ()


  join $ liftIO $ atomically $
    generateTagInstance <$> 
      pure tagType <*>
      MembersRegistry.getPairs memberValuesRegistry <*>
      MembersRegistry.getPairs memberEdgesRegistry <*>
      MembersRegistry.getPairs memberEventsRegistry <*>
      MembersRegistry.getPairs memberEventResultsRegistry <*>
      (toList <$> readTVar memberEventToMemberEventResultPairs)

generateTagInstance ::
  Type ->
  [(Type, Name)] ->
  [(Type, Name)] ->
  [(Type, Name)] ->
  [(Type, Name)] ->
  [(Name, Name)] ->
  AccumulateDecs ()
generateTagInstance tagType valueTable edgeTable eventTable eventResultTable eventToResultPairs =
  -- AccumulateDecs.liftDecsQ $ (:[]) <$> decQ
  -- where
  --   decQ = instanceD (return []) headQ decQs
  --     where
  --       headQ = appT [t| API.Tag |] (return tagType)
  --       decQs = [memberValueDecQ, undefined]
  --         where
  --           memberValueDecQ = undefined
  AccumulateDecs.liftDecsQ $ return [dec]
  where
    dec = InstanceD [] head decs
      where
        head = AppT (ConT ''API.Tag) tagType
        decs = [memberValueDec, undefined]
          where
            memberValueDec = DataD [] typeName [] [constructor] []
              where
                typeName = undefined
                constructor = undefined

isEdge :: Type -> Bool
isEdge = error "TODO: isEdge"

generateEvent :: Name -> [Type] -> AccumulateDecs (Type, Name)
generateEvent functionName argTypes = do
  AccumulateDecs.liftDecsQ $ return [declaration]
  return (ConT adtName, adtName)
  where
    declaration = DataD [] adtName [] [constructor] [''Generic, ''Eq]
      where
        constructor = NormalC adtName $ map ((IsStrict,)) argTypes
    adtName = mkName $ case nameBase functionName of
      [] -> []
      (x:xs) -> Char.toUpper x : xs

generateEventInstance :: Type -> Type -> Name -> Name -> [Type] -> Type -> Bool -> AccumulateDecs ()
generateEventInstance eventType tagType eventCons functionName argTypes resultType isWrite = do
  AccumulateDecs.liftDecsQ
    [d|
      instance API.Event $(return eventType) $(return tagType) where
        type EventResult $(return eventType) $(return tagType) = $(return resultType)
        eventTransaction = $eventTransactionLambdaQ
    |]
    where
      eventTransactionLambdaQ = lam1E pattern body
        where
          pattern = conP eventCons $ map varP argList
          body = appE constructor $ foldl appE (varE functionName) $ map varE argList
            where
              constructor = if isWrite then [e|API.Write|] else [e|API.Read|]
          argList = zipWith (\i _ -> mkName $ "_" ++ show i) [0..] argTypes

generateIsMemberEventOfInstance :: Type -> Type -> Name -> AccumulateDecs ()
generateIsMemberEventOfInstance eventType tagType memberEventCons = do
  AccumulateDecs.liftDecsQ
    [d|
      instance API.IsMemberEventOf $(return eventType) $(return tagType) where
        toMemberEvent = $(varE memberEventCons)
        fromMemberEvent = $fromMemberEventLambdaQ
    |]
    where
      -- FIXME: Should be a case with a Nothing result possible
      fromMemberEventLambdaQ = lam1E pattern body
        where
          pattern = conP memberEventCons $ [[p|z|]]
          body = appE (conE $ mkName "Just") (varE $ mkName "z")

generateIsMemberEventResultOfInstance :: Type -> Type -> Name -> AccumulateDecs ()
generateIsMemberEventResultOfInstance eventResultType tagType memberEventResultCons = do
  AccumulateDecs.liftDecsQ 
    [d|
      instance API.IsMemberEventResultOf $(return eventResultType) $(return tagType) where
        toMemberEventResult = $(varE memberEventResultCons)
        fromMemberEventResult = $fromMemberEventResultLambdaQ
    |]
    where
      -- FIXME: Should be a case with a Nothing result possible
      fromMemberEventResultLambdaQ = lam1E pattern body
        where
          pattern = conP memberEventResultCons [[p|z|]]
          body = appE (conE $ mkName "Just") (varE $ mkName "z")

generateEqInstance :: Type -> AccumulateDecs ()
generateEqInstance t = AccumulateDecs.liftDecsQ [d|deriving instance Eq $(return t)|]

generateShowInstance :: Type -> AccumulateDecs ()
generateShowInstance t = AccumulateDecs.liftDecsQ [d|deriving instance Show $(return t)|]

generateGenericInstance :: Type -> AccumulateDecs ()
generateGenericInstance t = AccumulateDecs.liftDecsQ [d|deriving instance Generic $(return t)|]

generateHashableInstance :: Type -> AccumulateDecs ()
generateHashableInstance t = AccumulateDecs.liftDecsQ [d|instance Hashable $(return t)|]

generateSafeCopyInstance :: Type -> AccumulateDecs ()
generateSafeCopyInstance t = AccumulateDecs.liftDecsQ $ case t of
  ConT name -> SafeCopy.deriveSafeCopy 0 'SafeCopy.base name

generateIsMemberEdgeOfInstance :: Type -> Type -> Name -> AccumulateDecs ()
generateIsMemberEdgeOfInstance edgeType tagType memberEdgeCons = 
  AccumulateDecs.liftDecsQ 
    [d|
      instance API.IsMemberEdgeOf $(return edgeType) $(return tagType) where
        toMemberEdge = $(varE memberEdgeCons)
        fromMemberEdge = $fromMemberEdgeLambdaQ
    |]
    where
      fromMemberEdgeLambdaQ = lamCaseE [match1, match2]
        where
          match1 = match pattern body []
            where
              pattern = conP memberEdgeCons [[p|z|]]
              body = normalB $ appE (conE $ mkName "Just") (varE $ mkName "z")
          match2 = match pattern body []
            where
              pattern = wildP
              body = normalB $ [e|Nothing|]

generateIsMemberValueOfInstance :: Type -> Type -> Name -> AccumulateDecs ()
generateIsMemberValueOfInstance valueType tagType memberValueCons = 
  AccumulateDecs.liftDecsQ 
    [d|
      instance API.IsMemberValueOf $(return valueType) $(return tagType) where
        toMemberValue = $(varE memberValueCons)
        fromMemberValue = $fromMemberValueLambdaQ
    |]
    where
      fromMemberValueLambdaQ = lamCaseE [match1, match2]
        where
          match1 = match pattern body []
            where
              pattern = conP memberValueCons [[p|z|]]
              body = normalB $ appE (conE $ mkName "Just") (varE $ mkName "z")
          match2 = match pattern body []
            where
              pattern = wildP
              body = normalB $ [e|Nothing|]



