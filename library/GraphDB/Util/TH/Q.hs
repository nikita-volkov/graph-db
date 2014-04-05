module GraphDB.Util.TH.Q where

import GraphDB.Util.Prelude
import Language.Haskell.TH
import qualified GraphDB.Util.TH.Type as Type
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Language.Haskell.TH.ExpandSyns as ExpandSyns


reifyLocalFunctions :: Q [(Name, Type)]
reifyLocalFunctions =
  listTopLevelFunctionLikeNames >>=
  mapM (\name -> reifyFunction name >>= mapM (return . (name, ))) >>=
  return . catMaybes
  where
    listTopLevelFunctionLikeNames = do 
      loc <- location
      text <- runIO $ Text.readFile $ loc_filename loc
      return $ map (mkName . Text.unpack) $ nub $ parse text
      where
        parse text = 
          either (error . ("Local function name parsing failure: " ++)) id $
          AP.parseOnly parser text
          where
            parser = 
              AP.sepBy (optional topLevelFunctionP <* AP.skipWhile (not . AP.isEndOfLine)) 
                       AP.endOfLine >>=
              return . catMaybes
              where
                topLevelFunctionP = do
                  head <- AP.satisfy Char.isLower
                  tail <- many (AP.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'']))
                  return $ Text.pack $ head : tail

reifyFunction :: Name -> Q (Maybe Type)
reifyFunction name = do
  tryToReify name >>= \case
    Just (VarI _ t _ _) -> return $ Just $ t
    _ -> return Nothing

reifyType :: Name -> Q (Maybe Type)
reifyType name = do
  tryToReify name >>= \case
    Just (TyConI _) -> Just <$> conT name
    _ -> return Nothing

expandRootSynType :: Type -> Q (Maybe Type)
expandRootSynType t = 
  case reverse $ Type.unapply t of
    ConT name : applications -> expandTypeSynonym name applications
    _ -> return Nothing

-- | 
-- Expand a type synonym identified by name, while providing the types to 
-- substitute its variables with.
-- 
-- Returns nothing, if it's not a type synonym. 
-- 
-- TODO:
-- Should fail if incorrect amount of args is provided, hence this function is only supposed
-- to be used as a helper for other functions, which unapply the type internally
-- and know exactly how many arguments to apply to it.
expandTypeSynonym :: Name -> [Type] -> Q (Maybe Type)
expandTypeSynonym name args = tryToReify name >>= return . join . fmap fromInfo
  where
    fromInfo = \case
      TyConI (TySynD _ vars t) -> 
        Just $ foldr ($) t $ do
          (var, arg) <- zip vars args
          return $ ExpandSyns.substInType ((tvName var), arg)
      _ -> Nothing
    tvName = \case
      PlainTV n -> n
      KindedTV n _ -> n

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 

purify :: Q a -> a
purify = unsafePerformIO . runQ

run :: MonadIO m => Q a -> m a
run = liftIO . runQ

caseLambda :: [Q Match] -> Q Exp
caseLambda matches = lamE [varP argName] (caseE (varE argName) matches)
  where
    argName = mkName "_0"


-- |
-- Only checks the instances in scope of the calling site,
-- it will not detect the declared instances, if they are not imported.
whenNoInstance :: Monoid a => Name -> [Type] -> Q a -> Q a
whenNoInstance name types f = do
  z <- recover (return False) (isInstance name types)
  if z
    then return mempty
    else f

liftSTM :: STM a -> Q a
liftSTM = runIO . atomically
