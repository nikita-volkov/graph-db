-- |
-- Pure computations on a reified data.
module GraphDB.Macros.Analysis where
 
import GraphDB.Util.Prelude
import GraphDB.Util.Prelude.TH
import qualified GraphDB.Util.TH.Type as TH
import qualified GraphDB.Model as M
import qualified GraphDB.Graph as G
import qualified GraphDB.Macros.Templates as T


type Root = Type
-- | A source and target type of an edge.
type EdgeInfo = (Type, Type)

decs :: Root -> [EdgeInfo] -> T.Decs
decs root infos =
  let
    assocs = edgeInfosToConAssocs root infos
    indexes = map conAssocToIndex assocs
    values = nub $ concatMap conAssocToValues assocs
    indexesFunctionClauses = map conAssocToIndexesClause assocs
    polyIndexInstances = map (\(c, t) -> (root, c, t)) indexes
    polyValueInstances = map (\(c, t) -> (root, c, t)) values
    hashableInstances = 
      nub $ concat $
        [
          ConT ''G.Index `AppT` root,
          ConT ''G.Value `AppT` root
        ] :
        indexTypes :
        valueTypes :
        leafTypes
      where
        indexTypes = indexes |> map sumConType
        valueTypes = values |> map sumConType
        leafTypes = map TH.monoTypes valueTypes
    serializableInstances = hashableInstances
    in 
      (
        polyIndexInstances,
        polyValueInstances,
        hashableInstances,
        serializableInstances,
        (
          root,
          indexes,
          values,
          indexesFunctionClauses
        )
      )

-- | An association of an index, source and target.
type ConAssoc = (T.SumConstructor, T.SumConstructor, T.SumConstructor)

edgeInfosToConAssocs :: Root -> [EdgeInfo] -> [ConAssoc]
edgeInfosToConAssocs root infos = 
  runST $ do
    regIndexType <- regType <$> pure indexPrefix <*> newSTRef 0 <*> newSTRef []
    regValueType <- regType <$> pure valuePrefix <*> newSTRef 0 <*> newSTRef []
    forM infos $ \(l, r) -> do
      let indexT = ConT ''M.Index `AppT` l `AppT` r
      index <- regIndexType indexT
      lc <- regValueType l
      rc <- regValueType r
      return (index, lc, rc)
  where
    regType prefix counterRef tableRef t = do
      readSTRef tableRef >>= return . lookup t >>= \case
        Just c -> return (c, t)
        Nothing -> do
          i <- readSTRef counterRef
          let c = mkName $ prefix <> show i 
          modifySTRef tableRef ((t, c) :)
          writeSTRef counterRef (succ i)
          return (c, t)
    rootName = case root of
      ConT n -> n
      _ -> $bug $ "Not a constructor type: " <> show root
    indexPrefix = "GraphDB_Index_" <> nameBase rootName <> "_"
    valuePrefix = "GraphDB_Value_" <> nameBase rootName <> "_"

conAssocToIndex :: ConAssoc -> T.SumConstructor
conAssocToIndex (i, _, _) = i

conAssocToValues :: ConAssoc -> [T.SumConstructor]
conAssocToValues (_, l, r) = [l, r]

conAssocToIndexesClause :: ConAssoc -> T.IndexesClause
conAssocToIndexesClause ((n, _), (n', _), (n'', _)) = (n, n', n'')

sumConType :: T.SumConstructor -> Type
sumConType (_, t) = t
