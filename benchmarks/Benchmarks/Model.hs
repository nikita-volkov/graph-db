-- |
-- A shared model.
-- Subjects provide implementation for 'Session'.
module Benchmarks.Model where

import Benchmarks.Prelude


-- * Values
-------------------------

newtype UID a = UID Int deriving (Show, Eq, Ord, Generic, Data, Typeable, Enum, Num, Real, Integral)
data Artist = Artist Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Genre = Genre Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
data Song = Song Name deriving (Show, Eq, Ord, Generic, Data, Typeable)
type Name = Text
data Identified a = Identified {-# UNPACK #-} !(UID a) !a deriving (Show, Eq, Ord, Generic, Data, Typeable)


-- * Session
-------------------------

type Session = FreeT SessionF
data SessionF a =
  InsertArtist Artist (UID Artist -> a) |
  InsertGenre Genre (UID Genre -> a) |
  InsertSong Song [UID Genre] [UID Artist] (UID Song -> a) |
  LookupArtistByUID (UID Artist) (Maybe (Identified Artist) -> a) |
  LookupArtistsByName Name ([Identified Artist] -> a) |
  LookupArtistsBySongGenreName Name ([Identified Artist] -> a)
  deriving (Functor)

makeFree ''SessionF
