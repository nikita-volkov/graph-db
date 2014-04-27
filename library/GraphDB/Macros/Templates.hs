module GraphDB.Macros.Templates where
 
import GraphDB.Util.Prelude
import GraphDB.Util.Prelude.TH
import qualified GraphDB.Graph as G
import qualified GraphDB.Model as M



data SetupInstance =
  SetupInstance {
    setup :: Type,
    algorithm :: Type,
    indexes :: [(Name, Type)],
    values :: [(Name, Type)],
    indexesFunctionClauses :: [IndexesClause]
  }

setupInstance :: SetupInstance -> Dec
setupInstance settings = 
  InstanceD
    []
    (AppT (ConT ''G.Setup) (setup settings))
    [
      TySynInstD ''G.Algorithm [setup settings] (algorithm settings),
      let
        constructors = do
          (n, t) <- indexes settings
          return $ NormalC n [(IsStrict, t)]
        derivations = [''Eq, ''Generic]
        in DataInstD [] ''G.Index [setup settings] constructors derivations,
      let
        constructors = do
          (n, t) <- values settings
          return $ NormalC n [(NotStrict, t)]
        derivations = [''Eq, ''Generic]
        in DataInstD [] ''G.Value [setup settings] constructors derivations,
      FunD 'G.indexes (map indexesClause (indexesFunctionClauses settings))
    ]



data IndexesClause =
  IndexesClause {
    indexConstructor :: Name,
    targetConstructor :: Name,
    sourceConstructor :: Name
  }

indexesClause :: IndexesClause -> Clause
indexesClause settings = 
  Clause 
    [
      ConP (targetConstructor settings) [VarP var1],
      ConP (sourceConstructor settings) [WildP]
    ]
    (NormalB exp)
    []
  where
    var1 = mkName "_1"
    exp = purify [e| map $(conE (indexConstructor settings)) $ M.indexes $(varE var1) |]



hashableInstance :: Type -> Dec
hashableInstance t = head $ purify [d| instance Hashable $(return t) |]



serializableInstance :: Type -> Dec
serializableInstance t = head $ purify [d| instance Serializable m $(return t) |]

