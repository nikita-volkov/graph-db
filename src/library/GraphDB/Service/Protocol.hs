{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Service.Protocol where

import GraphDB.Util.Prelude


data Request n v t =
  Request_Transaction (Request_Transaction_Spec n v t) |
  Request_StartTransaction Bool |
  Request_EndTransaction
  deriving (Generic)

instance (Serializable m n, Serializable m v, Serializable m t) => Serializable m (Request n v t)

data Request_Transaction_Spec n v t =
  Request_Transaction_Spec_GetRoot |
  Request_Transaction_Spec_NewNode v |
  Request_Transaction_Spec_GetTargetsByType n t
  deriving (Generic)

instance (Serializable m n, Serializable m v, Serializable m t) => Serializable m (Request_Transaction_Spec n v t)

data Response n v =
  Response_Transaction (Response_Transaction_Spec n v) |
  Response_StartTransaction |
  Response_EndTransaction
  deriving (Generic)

instance (Serializable m n, Serializable m v) => Serializable m (Response n v)

data Response_Transaction_Spec n v =
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

instance (Serializable m n, Serializable m v) => Serializable m (Response_Transaction_Spec n v)

