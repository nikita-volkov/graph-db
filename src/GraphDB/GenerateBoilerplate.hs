module GraphDB.GenerateBoilerplate where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified GraphDB.API as API
import qualified GraphDB.Graph.Transaction as Transaction
import qualified GraphDB.GenerateBoilerplate.MembersRegistry as MembersRegistry; import GraphDB.GenerateBoilerplate.MembersRegistry (MembersRegistry)
import qualified GraphDB.TH as TH
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.TH.Type as Type
import qualified CIO as CIO; import CIO (CIO)
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
      addDecs =<< generateValueOfInstance t tagType memberName
      addDecs =<< generateHashableInstance t
      addDecs =<< generateSerializableInstance t
    addEdgeType t = do
      memberName <- liftSTM $ MembersRegistry.resolve edgeMR t
      liftCIO $ TagInstanceBuilder.addMemberEdgeConstructor tib memberName t
      addDecs =<< generateEdgeOfInstance t tagType memberName
      addDecs =<< generateHashableInstance t
      addDecs =<< generateSerializableInstance t
    addValueOrEdgeType t = if isEdge t then addEdgeType t else addValueType t
    addTransactionFunction (name, argTypes, evResultType, isWrite) = do
      addDecs =<< generateEvent evName argTypes
      addDecs =<< generateSerializableInstance evType
      memberEventName <- liftSTM $ MembersRegistry.resolve eventMR evType
      addDecs =<< generateEventOfInstance evType tagType evName name argTypes evResultType isWrite memberEventName
      memberEventResultName <- liftSTM $ MembersRegistry.resolve eventResultMR evResultType
      addDecs =<< generateEventResultOfInstance evResultType tagType memberEventResultName
      liftCIO $ TagInstanceBuilder.addMemberEventConstructor tib memberEventName evType
      liftCIO $ TagInstanceBuilder.addMemberEventResultConstructor tib memberEventResultName evResultType
      liftCIO $ TagInstanceBuilder.addMemberEventTransactionClause tib memberEventName memberEventResultName
      where
        evName = mkName $ case nameBase name of
          x : xs -> Char.toUpper x : xs
          _ -> []
        evType = ConT evName
    in do
      do
        reifiedFunctions <- reifyLocalTransactionFunctions tagType
        when (null reifiedFunctions) $ error "No transaction-functions found in module"
        traverse_ addTransactionFunction reifiedFunctions
      do
        when (null valueTypeNames) $ error "No node-value types specified"
        traverse_ addValueType allValueTypes
      do
        edgeInstances <- reifyEdgeInstances allValueTypes
        when (null edgeInstances) $ error "No appropriate `Edge` instances found"
        traverse_ addEdgeType edgeInstances

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
  nub <$> getDecs

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
  Q.reifyLocalFunctions >>= fmap catMaybes . traverse getTransactionFunctionInfo
  where
    getTransactionFunctionInfo (name, argTypes, resultType) = 
      if all containsNoVarT argTypes
        then reifyTransaction resultType >>= return . join . fmap fromTransactionType
        else return Nothing
      where
        fromTransactionType (isWrite, trTagType, trResultType) = do
          assertZ $ trTagType == tagType
          return (name, argTypes, trResultType, isWrite)
        containsNoVarT = \case
          ForallT _ _ t -> containsNoVarT t
          AppT a b -> containsNoVarT a && containsNoVarT b
          VarT{} -> False
          _ -> True

reifyTransaction :: Type -> Q (Maybe (Bool, Type, Type))
reifyTransaction = \case
  trType `AppT` tagType `AppT` _ `AppT` resultType 
    | isNodeRef -> return Nothing
    | isTransaction -> return $ Just (trType == writeType, tagType, resultType)
    where
      isTransaction = trType `elem` [writeType, readType]
      isNodeRef = case Type.unapply resultType of
        _ : _ : _ : z : [] | z == nodeRefType -> True
        _ -> False
        where
          nodeRefType = Q.purify [t| API.NodeRef |]
      writeType = Q.purify [t| API.Write |]
      readType = Q.purify [t| API.Read |]
  -- A very special case for `Any` type-synonym:
  ForallT 
    [_, KindedTV tVarName _] 
    constraints 
    (AppT
      (AppT
        (AppT
          (AppT (VarT tVarName') (AppT memberValueType tagType))
          (AppT memberEdgeType tagType'))
        _)
      resultType)
    | memberValueType == ConT ''API.MemberValue,
      memberEdgeType == ConT ''API.MemberEdge,
      tVarName == tVarName',
      tagType == tagType',
      any ((== Just tVarName) . transactionConstraintVarName) constraints ->
    return $ Just (False, tagType, resultType)
    where
      transactionConstraintVarName = \case
        ClassP className [VarT varName] | className == ''Transaction.Transaction -> Just varName
        _ -> Nothing
  t@ForallT{} -> reifyTransaction $ Type.unforall t
  t -> Q.expandRootSynType t >>= fmap join . traverse reifyTransaction

-- |
-- Get a list of all instances of 'Edge' between all possible combinations of provided types.
reifyEdgeInstances :: [Type] -> Q [Type]
reifyEdgeInstances types = do
  decs <- fmap nub $ fmap join $ sequence $ do
    toType <- types
    return $ reifyInstances ''API.Edge [toType]
  return $ catMaybes $ map Type.fromDataInstanceDec decs

isEdge :: Type -> Bool
isEdge t = case Type.unapply t of
  _ : _ : t : [] | t == edgeT -> True
  _ -> False
  where
    edgeT = Q.purify [t| API.Edge |]

generateEvent :: Name -> [Type] -> Q [Dec]
generateEvent adtName argTypes = return [declaration]
  where
    declaration = DataD [] adtName [] [constructor] derivations
      where
        constructor = NormalC adtName $ map ((IsStrict,)) argTypes
        derivations = [''Eq, ''Generic]

generateEventOfInstance :: Type -> Type -> Name -> Name -> [Type] -> Type -> Bool -> Name -> Q [Dec]
generateEventOfInstance eventType tagType eventName functionName argTypes resultType isWrite memberEventName = 
  pure $ (:[]) $ InstanceD [] instanceHead decs
  where
    instanceHead = Type.apply [tagType, eventType, ConT ''API.EventOf]
    decs = [dec1, dec2, dec3, dec4]
      where
        dec1 = TySynInstD ''API.EventResult [eventType, tagType] resultType
        dec2 = FunD 'API.eventTransaction [clause]
          where
            clause = Clause [pattern] body []
              where
                pattern = ConP eventName $ map VarP argList
                body = 
                  NormalB $ AppE constructor $ foldl AppE (VarE functionName) $ map VarE argList
                  where
                    constructor = if isWrite then ConE 'API.Write else ConE 'API.Read
                argList = zipWith (\i _ -> mkName $ "_" ++ show i) [0..] argTypes
        dec3 = FunD 'API.toMemberEvent [clause]
          where
            clause = Clause [] (NormalB $ ConE memberEventName) []
        dec4 = FunD 'API.fromMemberEvent [clause]
          where
            clause = Clause [] (NormalB $ TH.caseLambda [match1, match2]) []
              where
                match1 = Match pattern body []
                  where
                    pattern = ConP memberEventName [VarP patternVarName]
                    patternVarName = mkName "_0"
                    body = NormalB $ Q.purify [e| Just $(varE patternVarName) |]
                match2 = Match pattern body []
                  where
                    pattern = WildP
                    body = NormalB $ Q.purify [e| Nothing |]

generateEventResultOfInstance :: Type -> Type -> Name -> Q [Dec]
generateEventResultOfInstance eventResultType tagType memberEventResultName =  
  [d|
    instance API.EventResultOf $(return eventResultType) $(return tagType) where
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
  Q.whenNoInstance ''Eq [t] $ [d|deriving instance Eq $(return t)|]

generateShowInstance :: Type -> Q [Dec]
generateShowInstance t = 
  Q.whenNoInstance ''Show [t] $ [d|deriving instance Show $(return t)|]

generateGenericInstance :: Type -> Q [Dec]
generateGenericInstance t = 
  Q.whenNoInstance ''Generic [t] $ [d|deriving instance Generic $(return t)|]

generateHashableInstance :: Type -> Q [Dec]
generateHashableInstance t = 
  Q.whenNoInstance ''Hashable [t] $ [d|instance Hashable $(return t)|]

generateSerializableInstance :: Type -> Q [Dec]
generateSerializableInstance t = 
  Q.whenNoInstance ''Serializable [ConT ''IO, t] 
    $ [d| instance Serializable IO $(return t) |]

generateEdgeOfInstance :: Type -> Type -> Name -> Q [Dec]
generateEdgeOfInstance edgeType tagType memberEdgeCons = 
  [d|
    instance API.EdgeOf $(return edgeType) $(return tagType) where
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

generateValueOfInstance :: Type -> Type -> Name -> Q [Dec]
generateValueOfInstance valueType tagType memberValueCons = 
  [d|
    instance API.ValueOf $(return valueType) $(return tagType) where
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

