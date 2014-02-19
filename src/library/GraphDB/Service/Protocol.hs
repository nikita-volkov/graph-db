{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Service.Protocol where

import GraphDB.Util.Prelude


data Request n v t i =
  Request_Transaction (Request_Transaction_Spec n v t i) |
  Request_StartTransaction Bool |
  Request_EndTransaction
  deriving (Generic)

instance (Serializable m n, Serializable m v, Serializable m t, Serializable m i) => 
         Serializable m (Request n v t i)

data Request_Transaction_Spec n v t i =
  Request_Transaction_Spec_GetRoot |
  Request_Transaction_Spec_NewNode v |
  Request_Transaction_Spec_GetTargetsByType n t |
  Request_Transaction_Spec_GetTargetsByIndex n i |
  Request_Transaction_Spec_AddTarget n n
  deriving (Generic)

instance (Serializable m n, Serializable m v, Serializable m t, Serializable m i) => 
         Serializable m (Request_Transaction_Spec n v t i)

data Response n v t i =
  Response_Transaction (Response_Transaction_Spec n v t i) |
  Response_StartTransaction |
  Response_EndTransaction
  deriving (Generic)

instance (Serializable m n, Serializable m v, Serializable m t, Serializable m i) => 
         Serializable m (Response n v t i)

data Response_Transaction_Spec n v t i =
  Response_Transaction_Spec_GetRoot n |
  Response_Transaction_Spec_NewNode n |
  Response_Transaction_Spec_GetTargetsByType [n] |
  Response_Transaction_Spec_GetTargetsByIndex [n] |
  Response_Transaction_Spec_AddTarget Bool |
  Response_Transaction_Spec_RemoveTarget Bool |
  Response_Transaction_Spec_GetValue v |
  Response_Transaction_Spec_SetValue |
  Response_Transaction_Spec_GetStats (Int, Int)
  deriving (Generic)

instance (Serializable m n, Serializable m v, Serializable m t, Serializable m i) => 
         Serializable m (Response_Transaction_Spec n v t i)

