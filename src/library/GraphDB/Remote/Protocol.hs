{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Remote.Protocol where

import GraphDB.Util.Prelude


data Request t =
  Request_Transaction (Request_Transaction_Spec t) Bool |
  Request_Event (Event t)
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Request t)

data Request_Transaction_Spec t =
  Request_Transaction_Spec_GetRoot |
  Request_Transaction_Spec_NewNode (Value t) |
  Request_Transaction_Spec_GetTargetsByType (Ref t) (Type t)
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Request_Transaction_Spec t)


data Response t =
  Response_Transaction (Response_Transaction_Spec t) |
  Response_Event (EventResult t)
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Response t)

data Response_Transaction_Spec t =
  Response_Transaction_Spec_GetRoot (Ref t) |
  Response_Transaction_Spec_NewNode (Ref t) |
  Response_Transaction_Spec_GetTargetsByType [Ref t]
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Response_Transaction_Spec t)


type family Event t
type family EventResult t
type family Value t
type family Ref t
type family Type t


type SerializableMembers m t = 
  (Serializable m (Event t),
   Serializable m (EventResult t),
   Serializable m (Value t),
   Serializable m (Ref t),
   Serializable m (Type t))


