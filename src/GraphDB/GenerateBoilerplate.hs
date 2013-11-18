module GraphDB.GenerateBoilerplate where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified GraphDB.API as API
import qualified GraphDB.GenerateBoilerplate.MembersRegistry as MembersRegistry; import GraphDB.GenerateBoilerplate.MembersRegistry (MembersRegistry)
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.TH.Type as Type
import qualified GraphDB.CIO as CIO; import GraphDB.CIO (CIO)
import qualified GraphDB.GenerateBoilerplate.TagInstanceBuilder as TagInstanceBuilder

-- |
-- Scan the current module for all transaction-functions and 
-- generate appropriate \"event\" data-types, 
-- associating them with the provided database tag and 
-- a list of supported types of node values.
-- 
-- All transaction-functions and edge declarations must be located 
-- in the same module where this macro gets called.
-- 
generateBoilerplate :: Name -> [Name] -> Q [Dec]
generateBoilerplate tagName valueTypeNames = do

  tagType <-
    Q.reifyType tagName >>= 
    return . fromMaybe (error $ "Not a type name: " ++ show tagName)

  valueTypes <- forM valueTypeNames $ \name -> 
    Q.reifyType name >>= return . fromMaybe (error $ "Not a type name: " ++ show name)
  allValueTypes <- do
    unitType <- [t| () |]
    return $ unitType : valueTypes
        
  (addDecs, getDecs) <- newDecsAccumulator
  tib <- liftCIO $ TagInstanceBuilder.new tagType

  (valueMR, edgeMR, eventMR, eventResultMR) <- 
    liftSTM $ (,,,) 
      <$> MembersRegistry.new (nameBase tagName ++ "_MemberValue_")
      <*> MembersRegistry.new (nameBase tagName ++ "_MemberEdge_")
      <*> MembersRegistry.new (nameBase tagName ++ "_MemberEvent_")
      <*> MembersRegistry.new (nameBase tagName ++ "_MemberEventResult_")

  writeType <- [t| API.Write |]
  readType <- [t| API.Read |]

  let
    addValueType t = do
      memberName <- liftSTM $ MembersRegistry.resolve valueMR t
      liftCIO $ TagInstanceBuilder.addMemberValueConstructor tib memberName t
      addDecs =<< generateIsMemberValueOfInstance t tagType memberName
      addDecs =<< generateHashableInstance t
      addDecs =<< generateSerializableInstance t
    addEdgeType t = do
      memberName <- liftSTM $ MembersRegistry.resolve edgeMR t
      liftCIO $ TagInstanceBuilder.addMemberEdgeConstructor tib memberName t
      addDecs =<< generateIsMemberEdgeOfInstance t tagType memberName
      addDecs =<< generateHashableInstance t
      addDecs =<< generateSerializableInstance t
    addValueOrEdgeType t = if isEdge t then addEdgeType t else addValueType t
    addTransactionFunction (name, argTypes, evResultType, isWrite) = do
      addDecs =<< generateEvent evName argTypes
      addDecs =<< generateEventInstance evType tagType evName name argTypes evResultType isWrite
      addDecs =<< generateSerializableInstance evType
      memberEventName <- liftSTM $ MembersRegistry.resolve eventMR evType
      addDecs =<< generateIsMemberEventOfInstance evType tagType memberEventName
      memberEventResultName <- liftSTM $ MembersRegistry.resolve eventResultMR evResultType
      addDecs =<< generateIsMemberEventResultOfInstance evResultType tagType memberEventResultName
      liftCIO $ TagInstanceBuilder.addMemberEventConstructor tib memberEventName evType
      liftCIO $ TagInstanceBuilder.addMemberEventResultConstructor tib memberEventResultName evResultType
      liftCIO $ TagInstanceBuilder.addMemberEventTransactionClause tib memberEventName memberEventResultName
      where
        evName = mkName $ case nameBase name of
          x : xs -> Char.toUpper x : xs
          _ -> []
        evType = ConT evName
    in do
      reifyLocalTransactionFunctions tagType >>= traverse_ addTransactionFunction
      traverse_ addValueType allValueTypes
      reifyEdgeInstances allValueTypes >>= traverse_ addEdgeType 

      [t| API.MemberEdge $(return tagType) |] 
        >>= applyAll [generateHashableInstance, generateSerializableInstance]
        >>= addDecs . join
      [t| API.MemberValue $(return tagType) |] 
        >>= applyAll [generateHashableInstance, generateSerializableInstance]
        >>= addDecs . join
      [t| API.MemberEvent $(return tagType) |]
        >>= applyAll [generateSerializableInstance]
        >>= addDecs . join
      [t| API.MemberEventResult $(return tagType) |]
        >>= applyAll [generateSerializableInstance]
        >>= addDecs . join

  addDecs =<< return . (:[]) =<< liftCIO (TagInstanceBuilder.getDec tib)
  getDecs

  where
    newDecsAccumulator = do
      decsVar <- liftSTM $ newTVar []
      return (addDecs decsVar, getDecs decsVar)
      where
        addDecs decsVar decs = liftSTM $ modifyTVar decsVar (decs ++)
        getDecs decsVar = liftSTM $ readTVar decsVar
    liftSTM = runIO . atomically
    liftCIO = runIO . CIO.run'
    liftIO = runIO

reifyLocalTransactionFunctions :: Type -> Q [(Name, [Type], Type, Bool)]
reifyLocalTransactionFunctions tagType = 
  Q.reifyLocalFunctions >>= return . catMaybes . map transactionFunctionInfo
  where
    transactionFunctionInfo (name, argTypes, resultType) = do
      assertZ isValid
      return (name, argTypes, trResultType, trType == writeType)
      where
        trResultType : stateThreadType : trTagType : trType : [] = Type.unapply resultType
        isValid = 
          trType `elem` [writeType, readType] && 
          trTagType == tagType &&
          not (isNodeRef trResultType)
          where
            isNodeRef t = case Type.unapply t of
              _ : _ : _ : z : [] | z == nodeRefType -> True
              _ -> False
              where
                nodeRefType = Q.purify [t| API.NodeRef |]
        writeType = Q.purify [t| API.Write |]
        readType = Q.purify [t| API.Read |]

-- |
-- Get a list of all instances of 'Edge' between all possible combinations of provided types.
reifyEdgeInstances :: [Type] -> Q [Type]
reifyEdgeInstances types = do
  ConT familyName <- [t| API.EdgeTo |]
  decs <- fmap join $ sequence $ do
    toType <- types
    return $ reifyInstances familyName [toType]
  return $ catMaybes $ map Type.fromDataInstanceDec decs

isEdge :: Type -> Bool
isEdge t = case Type.unapply t of
  _ : _ : t : [] | t == edgeT -> True
  _ -> False
  where
    edgeT = Q.purify [t| API.EdgeTo |]

generateEvent :: Name -> [Type] -> Q [Dec]
generateEvent adtName argTypes = return [declaration]
  where
    declaration = DataD [] adtName [] [constructor] derivations
      where
        constructor = NormalC adtName $ map ((IsStrict,)) argTypes
        derivations = map mkName ["Eq", "Generic"]

generateEventInstance :: Type -> Type -> Name -> Name -> [Type] -> Type -> Bool -> Q [Dec]
generateEventInstance eventType tagType eventCons functionName argTypes resultType isWrite = 
  pure $ (:[]) $ InstanceD [] head decs
  where
    head = Type.apply [tagType, eventType, ConT ''API.Event]
    decs = [dec1, dec2]
      where
        dec1 = TySynInstD (mkName "EventResult") [eventType, tagType] resultType
        dec2 = FunD (mkName "eventTransaction") [clause]
          where
            clause = Clause [pattern] body []
              where
                pattern = ConP eventCons $ map VarP argList
                body = 
                  NormalB $ AppE constructor $ foldl AppE (VarE functionName) $ map VarE argList
                  where
                    constructor = if isWrite then ConE 'API.Write else ConE 'API.Read
                argList = zipWith (\i _ -> mkName $ "_" ++ show i) [0..] argTypes

generateIsMemberEventOfInstance :: Type -> Type -> Name -> Q [Dec]
generateIsMemberEventOfInstance eventType tagType memberEventCons = 
  [d|
    instance API.IsMemberEventOf $(return eventType) $(return tagType) where
      toMemberEvent = $(conE memberEventCons)
      fromMemberEvent = $fromMemberEventLambdaQ
  |]
  where
    fromMemberEventLambdaQ = Q.caseLambda [pure match1, pure match2]
      where
        match1 = Match pattern body []
          where
            pattern = ConP memberEventCons [VarP patternVarName]
            patternVarName = mkName "_0"
            body = NormalB $ Q.purify [e| Just $(varE patternVarName) |]
        match2 = Match pattern body []
          where
            pattern = WildP
            body = NormalB $ Q.purify [e| Nothing |]

generateIsMemberEventResultOfInstance :: Type -> Type -> Name -> Q [Dec]
generateIsMemberEventResultOfInstance eventResultType tagType memberEventResultName =  
  [d|
    instance API.IsMemberEventResultOf $(return eventResultType) $(return tagType) where
      toMemberEventResult = $(conE memberEventResultName)
      fromMemberEventResult = $(fromMemberEventResultLambdaQ)
  |]
  where
    fromMemberEventResultLambdaQ = Q.caseLambda [pure match1, pure match2]
      where
        match1 = Match pattern body []
          where
            pattern = ConP memberEventResultName [VarP patternVarName]
            patternVarName = mkName "_0"
            body = NormalB $ Q.purify [e| Just $(varE patternVarName) |]
        match2 = Match pattern body []
          where
            pattern = WildP
            body = NormalB $ Q.purify [e| Nothing |]


generateEqInstance :: Type -> Q [Dec]
generateEqInstance t = 
  Q.whenNoInstance (mkName "Eq") [t] $ [d|deriving instance Eq $(return t)|]

generateShowInstance :: Type -> Q [Dec]
generateShowInstance t = 
  Q.whenNoInstance (mkName "Show") [t] $ [d|deriving instance Show $(return t)|]

generateGenericInstance :: Type -> Q [Dec]
generateGenericInstance t = 
  Q.whenNoInstance (mkName "Generic") [t] $ [d|deriving instance Generic $(return t)|]

generateHashableInstance :: Type -> Q [Dec]
generateHashableInstance t = 
  Q.whenNoInstance (mkName "Hashable") [t] $ [d|instance Hashable $(return t)|]

generateSerializableInstance :: Type -> Q [Dec]
generateSerializableInstance t = do
  Q.whenNoInstance (mkName "Serializable") [ConT $ mkName "IO", t] $
    [d| instance Serializable IO $(return t) |]

generateIsMemberEdgeOfInstance :: Type -> Type -> Name -> Q [Dec]
generateIsMemberEdgeOfInstance edgeType tagType memberEdgeCons = 
  [d|
    instance API.IsMemberEdgeOf $(return edgeType) $(return tagType) where
      toMemberEdge = $(conE memberEdgeCons)
      fromMemberEdge = $fromMemberEdgeLambdaQ
  |]
  where
    fromMemberEdgeLambdaQ = Q.caseLambda [pure match1, pure match2]
      where
        match1 = Match pattern body []
          where
            pattern = ConP memberEdgeCons [VarP patternVarName]
            patternVarName = mkName "_0"
            body = NormalB $ Q.purify [e| Just $(varE patternVarName) |]
        match2 = Match pattern body []
          where
            pattern = WildP
            body = NormalB $ Q.purify [e| Nothing |]

generateIsMemberValueOfInstance :: Type -> Type -> Name -> Q [Dec]
generateIsMemberValueOfInstance valueType tagType memberValueCons = 
  [d|
    instance API.IsMemberValueOf $(return valueType) $(return tagType) where
      toMemberValue = $(conE memberValueCons)
      fromMemberValue = $fromMemberValueLambdaQ
  |]
  where
    fromMemberValueLambdaQ = Q.caseLambda [pure match1, pure match2]
      where
        match1 = Match pattern body []
          where
            pattern = ConP memberValueCons [VarP patternVarName]
            patternVarName = mkName "_0"
            body = NormalB $ Q.purify [e| Just $(varE patternVarName) |]
        match2 = Match pattern body []
          where
            pattern = WildP
            body = NormalB $ Q.purify [e| Nothing |]

