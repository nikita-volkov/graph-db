{-# LANGUAGE UndecidableInstances #-}
module GraphDB.Remote.Protocol where

import GraphDB.Util.Prelude


data Request t =
  Request_Session (Request_Session_Spec t) |
  Request_StartSession (Maybe ByteString) |
  Request_CloseSession
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Request t)

data Request_Session_Spec t =
  Request_Session_Spec_Transaction (Request_Session_Spec_Transaction_Spec t) Bool |
  Request_Session_Spec_Event (Event t) |
  Request_Session_Spec_CheckIn
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Request_Session_Spec t)

data Request_Session_Spec_Transaction_Spec t =
  Request_Session_Spec_Transaction_Spec_GetRoot |
  Request_Session_Spec_Transaction_Spec_NewNode (Value t) |
  Request_Session_Spec_Transaction_Spec_GetTargetsByType (Ref t) (Type t)
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Request_Session_Spec_Transaction_Spec t)


data Response t =
  Response_Session (Either (Response_Session_Failure t) (Response_Session_Spec t)) |
  Response_StartSession Bool |
  Response_CloseSession
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Response t)

data Response_Session_Failure t =
  -- | The server is busy and suggests to retry after the specified amount of milliseconds.
  Response_Session_Failure_Busy Int |
  -- | The session got closed due to a keepalive timeout.
  Response_Session_Failure_Timeout
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Response_Session_Failure t)

data Response_Session_Spec t =
  Response_Session_Spec_Transaction (Response_Session_Spec_Transaction_Spec t) |
  Response_Session_Spec_Event (EventResult t) |
  Response_Session_CheckIn
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Response_Session_Spec t)

data Response_Session_Spec_Transaction_Spec t =
  Response_Session_Spec_Transaction_Spec_GetRoot (Ref t) |
  Response_Session_Spec_Transaction_Spec_NewNode (Ref t) |
  Response_Session_Spec_Transaction_Spec_GetTargetsByType [Ref t]
  deriving (Generic)

instance (SerializableMembers m t) => Serializable m (Response_Session_Spec_Transaction_Spec t)


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


