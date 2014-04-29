module GraphDB.Util.Prelude.TH
(
  module Exports,

  purify,
  tryToReify,
  isProperInstance',
)
where

import GraphDB.Util.Prelude hiding (Fixity)
import Language.Haskell.TH as Exports
import Language.Haskell.TH.Syntax as Exports
import THInstanceReification as Exports
import qualified Control.DeepSeq as DS
import qualified Control.DeepSeq.TH as DS


purify :: Q a -> a
purify = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 

isProperInstance' :: Name -> [Type] -> Q Bool
isProperInstance' name types = recover (return False) (isProperInstance name types)


-- * DeepSeq instances
-------------------------
 
DS.deriveNFDatas 
  [
    ''Name,
    ''Dec,
    ''Type,
    ''FamFlavour,
    ''Pragma,
    ''Fixity,
    ''Foreign,
    ''FunDep,
    ''Con,
    ''Body,
    ''Pat,
    ''Clause,
    ''TyLit,
    ''RuleBndr,
    ''Phases,
    ''RuleMatch,
    ''Inline,
    ''FixityDirection,
    ''Safety,
    ''Callconv,
    ''Pred,
    ''TyVarBndr,
    ''Strict,
    ''Guard,
    ''Lit,
    ''Stmt,
    ''Exp,
    ''Range,
    ''Match,
    ''NameSpace
  ]

deriving instance DS.NFData OccName
deriving instance DS.NFData ModName
deriving instance DS.NFData PkgName
instance DS.NFData NameFlavour where
  rnf = \case
    NameQ a -> DS.deepseq a ()
    NameG a b c -> DS.deepseq a (DS.deepseq b (DS.deepseq c ()))
    _ -> ()
