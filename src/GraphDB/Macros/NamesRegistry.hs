module GraphDB.Macros.NamesRegistry where

import GraphDB.Prelude
import Language.Haskell.TH
import qualified Data.Map as Map


data NamesRegistry = NamesRegistry {
  resolve :: Type -> STM Name,
  getTypes :: STM [Type],
  getNames :: STM [Name],
  getPairs :: STM [(Type, Name)]
}

new :: String -> STM NamesRegistry
new prefix = do
  tableVar <- newTVar []
  sizeVar <- newTVar 0
  return $ NamesRegistry (resolve tableVar sizeVar) (getTypes tableVar) (getNames tableVar) (getPairs tableVar)
  where
    resolve tableVar sizeVar ty = do
      table <- readTVar tableVar
      case lookup ty table of
        Nothing -> do
          size <- readTVar sizeVar
          let memberName = mkName $ prefix <> show size
          writeTVar tableVar $ (ty, memberName) : table
          modifyTVar sizeVar succ
          return memberName
        Just memberName -> return memberName
    getTypes tableVar = map fst <$> readTVar tableVar
    getNames tableVar = map snd <$> readTVar tableVar
    getPairs tableVar = readTVar tableVar

