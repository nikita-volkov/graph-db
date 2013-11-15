module GraphDB.GenerateBoilerplate.MembersRegistry where

import GraphDB.Prelude
import qualified GraphDB.API as API
import Language.Haskell.TH
import qualified Data.Map as Map


data MembersRegistry = MembersRegistry {
  resolve :: Type -> STM Name,
  getMembers :: STM [Name],
  getPairs :: STM [(Type, Name)]
}

new :: String -> STM MembersRegistry
new prefix = do
  tableVar <- newTVar []
  sizeVar <- newTVar 0
  return $ MembersRegistry (resolve tableVar sizeVar) (getMembers tableVar) (getPairs tableVar)
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
    getMembers tableVar = map snd <$> readTVar tableVar
    getPairs tableVar = readTVar tableVar

