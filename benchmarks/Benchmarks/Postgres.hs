-- |
-- This one requires a database "test" with access for user "postgres"
-- with no password.
-- The user must have appropriate privileges to drop and create tables
-- in that database.
module Benchmarks.Postgres where

import Benchmarks.Prelude
import Control.Lens
import qualified Benchmarks.Postgres.PostgreSQLSimplePlus as P


-- * Model
-------------------------

newtype UID a = UID Int deriving (Show, Eq, Generic, Ord, Enum, Num, Real, Integral, P.FromField, P.ToField)
data Artist = Artist Name deriving (Show, Eq, Generic)
data Genre = Genre Name deriving (Show, Eq, Generic)
data Song = Song Name deriving (Show, Eq, Generic)
type Name = Text
data Identified a = Identified {-# UNPACK #-} !(UID a) !a

instance P.FromRow Artist where 
  fromRow = Artist <$> P.field
instance P.ToRow Artist where 
  toRow (Artist n) = [P.toField n]
instance P.FromRow Genre where 
  fromRow = Genre <$> P.field
instance P.ToRow Genre where 
  toRow (Genre n) = [P.toField n]
instance P.FromRow Song where 
  fromRow = Song <$> P.field
instance P.ToRow Song where 
  toRow (Song n) = [P.toField n]
instance P.FromRow a => P.FromRow (Identified a) where 
  fromRow = Identified <$> P.field <*> P.fromRow
instance P.ToRow a => P.ToRow (Identified a) where 
  toRow (Identified uid a) = P.toField uid : P.toRow a


-- * Transactions
-------------------------

insertArtist :: Artist -> P.Action (UID Artist)
insertArtist artist = 
  fmap (P.fromOnly . head) $ 
  P.query "INSERT INTO artist (name) VALUES (?) RETURNING id" artist

insertGenre :: Genre -> P.Action (UID Genre)
insertGenre genre = 
  fmap (P.fromOnly . head) $ 
  P.query "INSERT INTO genre (name) VALUES (?) RETURNING id" genre

insertSong :: Song -> [UID Genre] -> [UID Artist] -> P.Action (UID Song)
insertSong song genreUIDs artistUIDs = do
  songUID <-
    fmap (P.fromOnly . head) $ 
    P.query "INSERT INTO song (name) VALUES (?) RETURNING id" song

  mapM_ (P.execute "INSERT INTO song_genre (id1, id2) VALUES (?, ?)") $
    zip (repeat songUID) genreUIDs

  mapM_ (P.execute "INSERT INTO song_artist (id1, id2) VALUES (?, ?)") $
    zip (repeat songUID) artistUIDs

  return songUID


-- * Setup
-------------------------

init :: P.Action ()
init = do
  cleanUp
  P.execute_
    [P.sql|
      CREATE TABLE "artist" (
        "id" BIGSERIAL,
        "name" varchar NOT NULL,
        PRIMARY KEY ("id")
      );
      CREATE INDEX "artist_name" ON "artist" USING btree("name");
      CREATE TABLE "genre" (
        "id" BIGSERIAL,
        "name" varchar NOT NULL,
        PRIMARY KEY ("id")
      );
      CREATE INDEX "genre_name" ON "genre" USING btree("name");
      CREATE TABLE "song" (
        "id" BIGSERIAL,
        "name" varchar NOT NULL,
        PRIMARY KEY ("id")
      );
      CREATE INDEX "song_name" ON "song" USING btree("name");
      CREATE TABLE "song_genre" (
        "id1" integer,
        "id2" integer,
        CONSTRAINT "id1" FOREIGN KEY ("id1") REFERENCES "song" ("id") ON DELETE CASCADE,
        CONSTRAINT "id2" FOREIGN KEY ("id2") REFERENCES "genre" ("id") ON DELETE CASCADE
      );
      CREATE TABLE "song_artist" (
        "id1" integer,
        "id2" integer,
        CONSTRAINT "id1" FOREIGN KEY ("id1") REFERENCES "song" ("id") ON DELETE CASCADE,
        CONSTRAINT "id2" FOREIGN KEY ("id2") REFERENCES "artist" ("id") ON DELETE CASCADE
      );
    |]

cleanUp :: P.Action ()
cleanUp = do
  P.execute_
    [P.sql|
      DROP TABLE IF EXISTS "song_artist";
      DROP TABLE IF EXISTS "song_genre";
      DROP TABLE IF EXISTS "song";
      DROP TABLE IF EXISTS "genre";
      DROP TABLE IF EXISTS "artist";
    |]

