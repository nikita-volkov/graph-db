module GraphDB.Macros.Templates where
 
import GraphDB.Util.Prelude
import GraphDB.Util.Prelude.TH
import qualified GraphDB.Graph as G
import qualified GraphDB.Model as M



type Decs = 
  (
    [PolyIndexInstance], 
    [PolyValueInstance],
    [HashableInstance], 
    [SerializableInstance], 
    SetupInstance
  )

renderDecs :: Decs -> [Dec]
renderDecs (a, b, c, d, e) =
  let
    a' = map renderPolyIndexInstance a
    b' = map renderPolyValueInstance b
    c' = map renderHashableInstance c
    d' = map renderSerializableInstance d
    e' = renderSetupInstance e
    in 
      e' : a' ++ b' ++ c' ++ d' 



type SetupInstance =
  (
    Type,
    [SumConstructor],
    [SumConstructor],
    [IndexesClause]
  )

renderSetupInstance :: SetupInstance -> Dec
renderSetupInstance (setup, indexes, values, indexesFunctionClauses) = 
  InstanceD
    []
    (AppT (ConT ''G.Setup) (setup))
    [
      TySynInstD ''G.Algorithm (TySynEqn [setup] (ConT ''G.Linear)),
      renderSumData ''G.Index setup IsStrict indexes,
      renderSumData ''G.Value setup NotStrict values,
      FunD 'G.indexes (map renderIndexesClause indexesFunctionClauses)
    ]



type SumConstructor = (Name, Type)

renderSumData :: Name -> Type -> Strict -> [SumConstructor] -> Dec
renderSumData name setup strict constructorsData =
  DataInstD [] name [setup] constructors derivations
  where
    constructors = do
      (n, t) <- constructorsData
      return $ NormalC n [(strict, t)]
    derivations = [''Eq, ''Generic]



type IndexesClause = (IndexConstructor, SourceConstructor, TargetConstructor)
type IndexConstructor = Name
type TargetConstructor = Name
type SourceConstructor = Name

renderIndexesClause :: IndexesClause -> Clause
renderIndexesClause (indexConstructor, sourceConstructor, targetConstructor) = 
  Clause 
    [
      ConP targetConstructor [VarP var1],
      ConP sourceConstructor [WildP]
    ]
    (NormalB exp)
    []
  where
    var1 = mkName "_1"
    exp = purify [e| map $(conE indexConstructor) $ M.indexes $(varE var1) |]



type HashableInstance = Type

renderHashableInstance :: HashableInstance -> Dec
renderHashableInstance t = head $ purify [d| instance Hashable $(return t) |]



type SerializableInstance = Type

renderSerializableInstance :: SerializableInstance -> Dec
renderSerializableInstance t = head $ purify [d| instance Serializable m $(return t) |]



type PolyValueInstance = (Type, Name, Type)

renderPolyValueInstance :: PolyValueInstance -> Dec
renderPolyValueInstance (st, c, t) =
  InstanceD [] head [packValue, unpackValue]
  where
    head = ConT ''M.PolyValue `AppT` st `AppT` t
    packValue = FunD 'M.packValue [clause]
      where
        clause = Clause [pattern] body []
          where
            pattern = VarP var
            var = mkName "_1"
            body = NormalB $ purify [e| $(conE c) $(varE var) |]
    unpackValue = FunD 'M.unpackValue [clause1, clause2]
      where
        clause1 = Clause [pattern] body []
          where
            pattern = ConP c [VarP var]
            var = mkName "_1"
            body = NormalB $ purify [e| Just $(varE var) |] 
        clause2 = Clause [pattern] body []
          where
            pattern = WildP
            body = NormalB $ purify [e| Nothing |]



type PolyIndexInstance = (Type, Name, Type)

renderPolyIndexInstance :: PolyIndexInstance -> Dec
renderPolyIndexInstance (setup, c, t) =
  head $ purify $
    [d| 
      instance M.PolyIndex $(pure setup) $(pure t) where
        packIndex = $(conE c)
    |]
