module HTFTestSuite.A where

import GraphDB.Util.Prelude


data A = A { ref :: IORef Float }
  deriving (Generic)

instance Serializable IO A

data Event = Increase | Decrease | Multiply Float | Divide Float | Get
  deriving (Show, Ord, Eq, Generic)

instance Serializable m Event

data EventResult = UnitEventResult () | FloatEventResult Float 
  deriving (Show, Ord, Eq, Generic)

instance Serializable m EventResult


initValue = A <$> newIORef 0

applyEvent (A ref) e = case e of
  Increase -> modifyIORef ref succ
  Decrease -> modifyIORef ref pred
  Multiply by -> modifyIORef ref (*by)
  Divide by -> modifyIORef ref (/by)

processRequestData (A ref) event = case event of
  Increase -> modifyIORef ref succ >> readIORef ref >>= return . FloatEventResult
  Decrease -> modifyIORef ref pred >> readIORef ref >>= return . FloatEventResult
  Multiply by -> modifyIORef ref (*by) >> readIORef ref >>= return . FloatEventResult
  Divide by -> modifyIORef ref (/by) >> readIORef ref >>= return . FloatEventResult
  Get -> readIORef ref >>= return . FloatEventResult
