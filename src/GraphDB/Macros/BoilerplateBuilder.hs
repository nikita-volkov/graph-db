module GraphDB.Macros.BoilerplateBuilder where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified GraphDB.API as API
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.TH as TH
import qualified GraphDB.TH.Q as Q
import qualified GraphDB.TH.Type as Type
import qualified GraphDB.Macros.NamesRegistry as NamesRegistry
import qualified GraphDB.Macros.TagInstanceBuilder as TagInstanceBuilder
import qualified GraphDB.Macros.TagInstanceBuilder as TagInstanceBuilder
import qualified Data.Char as Char

data BoilerplateBuilder = BoilerplateBuilder {
  addReachablePair :: (Type, Type) -> Q (),
  addTransactionFunction :: (Name, [Type], Type, Bool) -> Q (),
  getDecs :: Q [Dec]
}

new :: (Name, Type) -> Q BoilerplateBuilder
new (tagName, tagType) = do

  graphDBTagInstBldr <- TagInstanceBuilder.new (tagName, tagType)
  graphTagInstBldr <- TagInstanceBuilder.new (tagName, tagType)

  (addDecs, getDecs) <- newDecsAccumulator

  [t| API.UnionValue $(return tagType) |] 
    >>= applyAll [generateHashableInstance, generateSerializableInstance]
    >>= addDecs . join
  [t| API.UnionValueType $(return tagType) |] 
    >>= applyAll [generateHashableInstance, generateSerializableInstance]
    >>= addDecs . join
  [t| API.UnionEvent $(return tagType) |]
    >>= applyAll [generateSerializableInstance]
    >>= addDecs . join
  [t| API.UnionEventResult $(return tagType) |]
    >>= applyAll [generateSerializableInstance]
    >>= addDecs . join

  let
    addReachablePair (source, target) = do
      addDecs =<< generateHashableInstance =<< [t| API.Index $(pure tagType) $(pure source) $(pure target) |]
      addValue source
      addValue target
    addValue t = do
      (uvName, uvtName) <- TagInstanceBuilder.addValue graphTagInstBldr t
      addDecs =<< generateIsUnionValueInstance t tagType uvName
      addDecs =<< generateHashableInstance t
      addDecs =<< generateSerializableInstance t
    addTransactionFunction (name, argTypes, evResultType, isWrite) = do
      (memberEventName, memberEventResultName) <- 
        TagInstanceBuilder.addEventAndEventResult graphDBTagInstBldr (evType, evResultType)
      addDecs =<< generateEvent evName argTypes
      addDecs =<< generateSerializableInstance evType
      addDecs =<< generateIsUnionEventInstance evType tagType evName name argTypes evResultType isWrite memberEventName
      addDecs =<< generateIsUnionEventResultInstance evResultType tagType memberEventResultName
      where
        evName = mkName $ case nameBase name of
          x : xs -> Char.toUpper x : xs
          _ -> []
        evType = ConT evName
    getDecs' = do
      decs <- getDecs
      dec1 <- TagInstanceBuilder.getDec graphDBTagInstBldr
      dec2 <- TagInstanceBuilder.render graphTagInstBldr
      return $ dec1 : dec2 : decs

  return $ 
    BoilerplateBuilder 
      addReachablePair
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

generateIsUnionEventInstance :: Type -> Type -> Name -> Name -> [Type] -> Type -> Bool -> Name -> Q [Dec]
generateIsUnionEventInstance eventType tagType eventName functionName argTypes resultType isWrite memberEventName = 
  pure $ (:[]) $ InstanceD [] instanceHead decs
  where
    instanceHead = Type.apply [eventType, tagType, ConT ''API.IsUnionEvent]
    decs = [dec1, dec2, dec3, dec4]
      where
        dec1 = TySynInstD ''API.EventResult [tagType, eventType] resultType
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
        dec3 = FunD 'API.toUnionEvent [clause]
          where
            clause = Clause [] (NormalB $ ConE memberEventName) []
        dec4 = FunD 'API.fromUnionEvent [clause]
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

generateIsUnionEventResultInstance :: Type -> Type -> Name -> Q [Dec]
generateIsUnionEventResultInstance eventResultType tagType memberEventResultName =  
  [d|
    instance API.IsUnionEventResult $(return tagType) $(return eventResultType) where
      toUnionEventResult = $(conE memberEventResultName)
      fromUnionEventResult = $(fromUnionEventResultLambdaQ)
  |]
  where
    fromUnionEventResultLambdaQ = Q.caseLambda [pure match1, pure match2]
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

generateHashableInstance :: Type -> Q [Dec]
generateHashableInstance t = 
  Q.whenNoInstance ''Hashable [t] $ 
    [d| instance Hashable $(return t) |]

generateSerializableInstance :: Type -> Q [Dec]
generateSerializableInstance t = 
  Q.whenNoInstance ''Serializable [ConT ''IO, t] $ 
    [d| instance Serializable IO $(return t) |]

generateIsUnionValueInstance :: Type -> Type -> Name -> Q [Dec]
generateIsUnionValueInstance valueType tagType unionValueName = 
  [d|
    instance API.IsUnionValue $(return tagType) $(return valueType) where
      toUnionValue = $(conE unionValueName)
  |]
