module GraphDB.Model.Macros where
 
import GraphDB.Util.Prelude
import qualified Language.Haskell.TH as TH
import qualified GraphDB.Util.TH as THU
import qualified GraphDB.Util.TH.Q as Q
import qualified GraphDB.Model.Union as Union
import qualified GraphDB.Model.Edge as Edge


type Settings = (UnionType, Indexes, Values)
type UnionType = TH.Type
type Indexes = [TH.Type]
type Values = [TH.Type]


instances :: Settings -> [TH.Dec]
instances (unionType, polyIndexes, polyValues) = 
  unionInstance : polyValueInstances ++ polyIndexInstances
  where
    unionName = case unionType of
      TH.ConT n -> n
      _ -> $(bug "Not a constructor type")
    polyIndexToIndexList = do
      (polyIndex, i) <- polyIndexes `zip` [0..]
      let cn = TH.mkName $ "Union_" <> TH.nameBase unionName <> "_Index_" <> show i
      return (polyIndex, cn)
    polyValueToValueToTypeList = do
      (polyValue, i) <- polyValues `zip` [0..]
      let value = TH.mkName $ "Union_" <> TH.nameBase unionName <> "_Value_" <> show i 
          t = TH.mkName $ "Union_" <> TH.nameBase unionName <> "_Type_" <> show i
      return (polyValue, (value, t))
    unionInstance = TH.InstanceD [] head decs where
      head = TH.AppT (TH.ConT ''Union.Union) unionType
      decs = [indexTD, valueTD, typeTD, indexesFD, indexTargetTypeFD, decomposeValueFD, composeValueFD] where
        indexTD = TH.DataInstD [] ''Union.Index [unionType] constructors derivations where
          constructors = 
            flip map polyIndexToIndexList $ \case 
              (pit, icn) -> TH.NormalC icn [(TH.IsStrict, pit)]
          derivations = [''Eq, ''Generic]
        valueTD = TH.DataInstD [] ''Union.Value [unionType] constructors derivations where
          constructors = 
            flip map polyValueToValueToTypeList $ \case 
              (pvt, (vcn, tcn)) -> TH.NormalC vcn [(TH.NotStrict, pvt)]
          derivations = [''Eq, ''Generic]
        typeTD = TH.DataInstD [] ''Union.Type [unionType] constructors derivations where
          constructors = 
            flip map polyValueToValueToTypeList $ \case 
              (pvt, (vcn, tcn)) -> TH.NormalC tcn []
          derivations = [''Eq, ''Generic]
        indexesFD = TH.FunD 'Union.indexes clauses where
          clauses = flip map polyIndexToIndexList $ \case
            (pit, icn) -> TH.Clause [TH.ConP targetValueCN [TH.VarP var], 
                                     TH.ConP sourceTypeCN []] 
                                    (TH.NormalB exp) [] where
              (sourceT, targetT) = case pit of
                _ `TH.AppT` source `TH.AppT` target -> (source, target)
                _ -> $(bug "Unexpected type of index")
              sourceTypeCN = lookup sourceT polyValueToValueToTypeList |> \case
                Just (_, cn) -> cn
                _ -> $(bug "Poly value type not found")
              targetValueCN = lookup targetT polyValueToValueToTypeList |> \case
                Just (cn, _) -> cn
                _ -> $(bug "Poly value type not found")
              var = TH.mkName "_0"
              exp = Q.purify [e| map Union.packIndex (Edge.indexes $(TH.varE var) :: [$(pure pit)]) |]
        indexTargetTypeFD = TH.FunD 'Union.indexTargetType clauses where
          clauses = flip map polyIndexToIndexList $ \case
            (pit, icn) -> TH.Clause [TH.ConP icn [TH.WildP]] (TH.NormalB exp) [] where
              exp = TH.ConE targetTypeCN where
                (sourceT, targetT) = case pit of
                  _ `TH.AppT` source `TH.AppT` target -> (source, target)
                  _ -> $(bug "Unexpected type of index")
                targetTypeCN = lookup targetT polyValueToValueToTypeList |> \case
                  Just (_, cn) -> cn
                  _ -> $(bug "Poly value type not found")
        decomposeValueFD = TH.FunD 'Union.decomposeValue clauses where
          clauses = map clause polyValueToValueToTypeList where
            clause (pvt, (vcn, tcn)) = TH.Clause [TH.ConP vcn [TH.VarP var]] (TH.NormalB exp) [] where
              var = TH.mkName "_0"
              exp = Q.purify [e| ($(TH.conE tcn), unsafeCoerce $(TH.varE var)) |]
        composeValueFD = TH.FunD 'Union.composeValue clauses where
          clauses = map clause polyValueToValueToTypeList where
            clause (pvt, (vcn, tcn)) = TH.Clause [TH.ConP tcn [], TH.VarP var] (TH.NormalB exp) [] where
              var = TH.mkName "_0"
              exp = Q.purify [e| $(TH.conE vcn) $ unsafeCoerce $(TH.varE var) |]
    polyValueInstances = flip map polyValueToValueToTypeList $ \case
      (pvt, (vc, tc)) -> TH.InstanceD [] head decs where
        head = TH.ConT ''Union.PolyValue `TH.AppT` unionType `TH.AppT` pvt
        decs = [packValue, unpackValue] where
          packValue = TH.FunD 'Union.packValue [clause] where
            clause = TH.Clause [pattern] body [] where
              pattern = TH.VarP var
              var = TH.mkName "_0"
              body = TH.NormalB $ Q.purify [e| ($(TH.conE tc), $(TH.conE vc) $(TH.varE var)) |]
          unpackValue = TH.FunD 'Union.unpackValue [clause1, clause2] where
            clause1 = TH.Clause [pattern] body [] where
              pattern = TH.ConP vc [TH.VarP var]
              var = TH.mkName "_0"
              body = TH.NormalB $ Q.purify [e| Just $(TH.varE var) |]
            clause2 = TH.Clause [pattern] body [] where
              pattern = TH.WildP
              body = TH.NormalB $ Q.purify [e| Nothing |]
    polyIndexInstances = 
      flip map polyIndexToIndexList $ \(pit, ic) -> head $ Q.purify 
        [d| instance Union.PolyIndex $(pure unionType) $(pure pit) where 
              packIndex = $(TH.conE ic) |]

-- | 
-- Generate all the boilerplate code by the type of a root node's value.
-- 
-- This function resolves all related types from the 'Edge.Edge' instances
-- declared in the module it is being called in.
generateUnion :: TH.Name -> TH.Q [TH.Dec]
generateUnion name = do
  localInstances <- THU.reifyLocalInstances
  let edgePairs = do
        (cn, [a, b]) <- localInstances
        guard $ cn == ''Edge.Edge
        return (a, b)
      indexes = edgePairs |> map (\(s, t) -> TH.ConT ''Edge.Index `TH.AppT` s `TH.AppT` t)
      values = unzip edgePairs |> uncurry (++) |> nub
      unionType = TH.ConT name
  stdInstances <- 
    (indexes ++ values) |> 
    (TH.ConT ''Union.Index `TH.AppT` unionType :) |>
    (TH.ConT ''Union.Value `TH.AppT` unionType :) |>
    (TH.ConT ''Union.Type `TH.AppT` unionType :) |>
    mapM generateStdInstances |> 
    fmap join
  return $ stdInstances ++ instances (unionType, indexes, values)

generateStdInstances :: TH.Type -> TH.Q [TH.Dec]
generateStdInstances t = do
  a <- generateHashableInstance t
  b <- generateSerializableInstance t
  return $ a ++ b

generateHashableInstance :: TH.Type -> TH.Q [TH.Dec]
generateHashableInstance t = 
  Q.whenNoInstance ''Hashable [t] $ 
    [d| instance Hashable $(return t) |]

generateSerializableInstance :: TH.Type -> TH.Q [TH.Dec]
generateSerializableInstance t = 
  Q.whenNoInstance ''Serializable [TH.ConT ''IO, t] $ 
    [d| instance Serializable IO $(return t) |]

