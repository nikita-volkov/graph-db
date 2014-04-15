-- |
-- This one requires a database "test" with access for user "postgres"
-- with no password.
-- The user must have appropriate privileges to drop and create tables
-- in that database.
module Benchmarks.Postgres where

import Benchmarks.Prelude
import Control.Lens
import Benchmarks.Model
import qualified Benchmarks.Postgres.PostgreSQLSimplePlus as P


-- * Model
-------------------------

deriving instance P.FromField (UID a)
deriving instance P.ToField (UID a)

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

interpretSession :: (MonadIO m) => Session m a -> P.Session m a
interpretSession = iterTM $ \case
  InsertArtist artist continue ->
    (>>= continue) $
    P.runAction False $
    fmap (P.fromOnly . head) $ 
    P.query "INSERT INTO artist (name) VALUES (?) RETURNING id" artist
  InsertGenre genre continue -> 
    (>>= continue) $
    P.runAction False $
    fmap (P.fromOnly . head) $ 
    P.query "INSERT INTO genre (name) VALUES (?) RETURNING id" genre
  InsertSong song genreUIDs artistUIDs continue -> 
    (>>= continue) $ 
    P.runAction True $ do
      songUID <-
        fmap (P.fromOnly . head) $ 
        P.query "INSERT INTO song (name) VALUES (?) RETURNING id" song

      mapM_ (P.execute "INSERT INTO song_genre (id1, id2) VALUES (?, ?)") $
        zip (repeat songUID) genreUIDs

      mapM_ (P.execute "INSERT INTO song_artist (id1, id2) VALUES (?, ?)") $
        zip (repeat songUID) artistUIDs

      return songUID
  LookupArtistByUID uid continue ->
    P.runAction False (fmap listToMaybe $ P.query sql (P.Only uid)) >>= continue
    where
      sql = "SELECT * FROM artist WHERE id = ?"
  LookupArtistsByName name continue ->
    P.runAction False (P.query sql (P.Only name)) >>= continue
    where
      sql = "SELECT * FROM artist WHERE name = ?"
  LookupArtistsBySongGenreName name continue -> 
    P.runAction False (P.query sql (P.Only name)) >>= continue
    where
      sql = 
        [P.sql|
          SELECT *
            FROM artist
              LEFT JOIN song_artist ON song_artist.id2 = artist.id
              LEFT JOIN song_genre ON song_genre.id1 = song_artist.id1
              LEFT JOIN genre ON genre.id = song_genre.id2
            WHERE 
              genre.name = ?
        |]


-- * Setup
-------------------------

init :: (MonadIO m) => P.Session m ()
init = 
  P.runAction True $ P.execute_
    [P.sql|
      DROP TABLE IF EXISTS "song_artist";
      DROP TABLE IF EXISTS "song_genre";
      DROP TABLE IF EXISTS "song";
      DROP TABLE IF EXISTS "genre";
      DROP TABLE IF EXISTS "artist";
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

runSession :: (MonadIO m) => P.PoolSize -> P.Session m r -> m r
runSession poolSize sess = do
  P.runSession (host, port, user, pw, db, poolSize) $ sess
  where
    host = "localhost"
    port = 5432
    user = "postgres"
    pw = ""
    db = "test"


