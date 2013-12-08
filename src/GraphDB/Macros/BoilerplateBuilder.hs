module GraphDB.Macros.BoilerplateBuilder where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.Engine as Engine
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.TH as TH
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.TH.Type as Type
import qualified GraphDB.Macros.TagInstanceBuilder as TIB
import qualified Data.Char as Char
import qualified Data.Set as Set

data BoilerplateBuilder = BoilerplateBuilder {
  addEdge :: (Type, Type) -> Q (),
  addTransactionFunction :: (Name, [Type], Type, Bool) -> Q (),
  getDecs :: Q [Dec]
}

new :: (Name, Type) -> Q BoilerplateBuilder
new (tagName, tagType) = do

  tib <- TIB.new (tagName, tagType)

  (addDecs, getDecs) <- newDecsAccumulator

  [t| Engine.UnionIndex $(return tagType) |] 
    >>= applyAll [generateHashableInstance, generateSerializableInstance]
    >>= addDecs . join
  [t| Engine.UnionValue $(return tagType) |] 
    >>= applyAll [generateHashableInstance, generateSerializableInstance]
    >>= addDecs . join
  [t| Engine.UnionValueType $(return tagType) |] 
    >>= applyAll [generateHashableInstance, generateSerializableInstance]
    >>= addDecs . join
  [t| Engine.UnionEvent $(return tagType) |]
    >>= applyAll [generateSerializableInstance]
    >>= addDecs . join
  [t| Engine.UnionEventResult $(return tagType) |]
    >>= applyAll [generateSerializableInstance]
    >>= addDecs . join

  seenValueTypesRef <- runIO $ newIORef []

  let
    addEdge (source, target) = do
      indexType <- [t| Engine.Index $(pure tagType) $(pure source) $(pure target) |]
      unionIndexName <- TIB.addIndex tib indexType
      addDecs =<< generateHashableInstance indexType
      addDecs =<< generateSerializableInstance indexType
      addDecs =<< generateIndexInstance indexType tagType unionIndexName
      addValue source
      addValue target
    addValue t = do
      seenValueTypes <- runIO $ readIORef seenValueTypesRef
      when (not $ elem t seenValueTypes) $ do
        (uvName, utName) <- TIB.addValue tib t
        addDecs =<< generateValueInstance t tagType uvName utName
        addDecs =<< generateHashableInstance t
        addDecs =<< generateSerializableInstance t
        runIO $ modifyIORef seenValueTypesRef (t:)

    addTransactionFunction (name, argTypes, evResultType, isWrite) = do
      (unionEventName, unionEventResultName) <- 
        TIB.addEventAndEventResult tib (evType, evResultType)
      addDecs =<< generateEvent evName argTypes
      addDecs =<< generateSerializableInstance evType
      addDecs =<< generateEventInstance evType tagType evName name argTypes evResultType isWrite unionEventName
      addDecs =<< generateEventResultInstance evResultType tagType unionEventResultName
      where
        evName = mkName $ case nameBase name of
          x : xs -> Char.toUpper x : xs
          _ -> []
        evType = ConT evName
    getDecs' = (:) <$> TIB.render tib <*> getDecs

  return $ 
    BoilerplateBuilder 
      addEdge
      addTransactionFunction
      getDecs'

newDecsAccumulator :: Q ([Dec] -> Q (), Q [Dec])
newDecsAccumulator = do
  decsVar <- runIO $ newIORef []
  return (addDecs decsVar, getDecs decsVar)
  where
    addDecs decsVar decs = runIO $ modifyIORef decsVar (decs ++)
    getDecs decsVar = runIO $ nub <$> readIORef decsVar

generateEvent :: Name -> [Type] -> Q [Dec]
generateEvent adtName argTypes = return [declaration]
  where
    declaration = DataD [] adtName [] [constructor] derivations
      where
        constructor = NormalC adtName $ map ((IsStrict,)) argTypes
        derivations = [''Eq, ''Generic]

generateEventInstance :: Type -> Type -> Name -> Name -> [Type] -> Type -> Bool -> Name -> Q [Dec]
generateEventInstance eventType tagType eventName functionName argTypes resultType isWrite unionEventName = 
  pure $ (:[]) $ InstanceD [] instanceHead decs
  where
    instanceHead = Type.apply [eventType, tagType, ConT ''Engine.PolyEvent]
    decs = [dec1, dec2, dec3]
      where
        dec1 = TySynInstD ''Engine.PolyEvent_Result [tagType, eventType] resultType
        dec2 = FunD 'Engine.eventFinalTransaction [clause]
          where
            clause = Clause [pattern] body []
              where
                pattern = ConP eventName $ map VarP argList
                body = 
                  NormalB $ AppE constructor $ foldl AppE (VarE functionName) $ map VarE argList
                  where
                    constructor = if isWrite 
                      then ConE 'Engine.FinalTransaction_Write 
                      else ConE 'Engine.FinalTransaction_Read
                argList = zipWith (\i _ -> mkName $ "_" ++ show i) [0..] argTypes
        dec3 = FunD 'Engine.packEvent [clause]
          where
            clause = Clause [] (NormalB $ ConE unionEventName) []

generateEventResultInstance :: Type -> Type -> Name -> Q [Dec]
generateEventResultInstance eventResultType tagType unionEventResultName =  
  [d|
    instance Engine.PolyEventResult $(return tagType) $(return eventResultType) where
      packEventResult = $(conE unionEventResultName)
      unpackEventResult = $(fromUnionEventResultLambdaQ)
  |]
  where
    fromUnionEventResultLambdaQ = Q.caseLambda [pure match1, pure match2]
      where
        match1 = Match pattern body []
          where
            pattern = ConP unionEventResultName [VarP patternVarName]
            patternVarName = mkName "_0"
            body = NormalB $ Q.purify [e| Just $(varE patternVarName) |]
        match2 = Match pattern body []
          where
            pattern = WildP
            body = NormalB $ Q.purify [e| Nothing |]

generateHashableInstance :: Type -> Q [Dec]
generateHashableInstance t = 
  Q.whenNoInstance ''Hashable [t] $ 
    [d| instance Hashable $(return t) |]

generateSerializableInstance :: Type -> Q [Dec]
generateSerializableInstance t = 
  Q.whenNoInstance ''Serializable [ConT ''IO, t] $ 
    [d| instance Serializable IO $(return t) |]

generateValueInstance :: Type -> Type -> Name -> Name -> Q [Dec]
generateValueInstance valueType tagType unionValueName unionTypeName = 
  [d|
    instance Engine.PolyValue $(return tagType) $(return valueType) where
      packValue v = ($(conE unionTypeName), $(conE unionValueName) v)
      unpackValue = $(Q.caseLambda [pure match1, pure match2])
  |]
  where
    match1 = Match pattern body []
      where
        pattern = ConP unionValueName [VarP patternVarName]
        patternVarName = mkName "_0"
        body = NormalB $ Q.purify [e| Just $(varE patternVarName) |]
    match2 = Match pattern body []
      where
        pattern = WildP
        body = NormalB $ Q.purify [e| Nothing |]

generateIndexInstance :: Type -> Type -> Name -> Q [Dec]
generateIndexInstance indexType tagType unionIndexName = 
  [d|
    instance Engine.PolyIndex $(return tagType) $(return indexType) where
      packIndex = $(conE unionIndexName)
  |]
