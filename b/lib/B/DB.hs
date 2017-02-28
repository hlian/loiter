{-# LANGUAGE OverloadedStrings #-}

module B.DB where

import B.Prelude hiding ((.=))
import Data.Aeson (Value, ToJSON(..), (.=), (.:))
import qualified Data.Aeson as Aeson
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as SQL

data JobStatus
  = Alive
  | Done
  | Failed
  deriving (Show, Enum)

data Job =
  Job Int
      Value
      JobStatus
      UTCTime
  deriving (Show)

newtype Book =
  Book String

newtype Channel =
  Channel Text

data AbsorbChannel =
  AbsorbChannel Book
                Channel

instance Aeson.ToJSON AbsorbChannel where
  toJSON (AbsorbChannel (Book book) (Channel channel)) =
    Aeson.object ["b" .= book, "c" .= channel, "t" .= ("ChannelName" :: String)]

instance Aeson.FromJSON AbsorbChannel where
  parseJSON =
    Aeson.withObject "AbsorbChannel" $ \o -> do
      b <- o .: "b"
      c <- o .: "c"
      pure (AbsorbChannel (Book b) (Channel c))

type Cn = Pool SQL.Connection

connect :: IO Cn
connect = Pool.createPool birth death 1 1 10
  where
    birth = do
      conn <-
        SQL.connectPostgreSQL
          "host='127.0.0.1' port=5432 dbname='loiter' user='loiter' password='loiter'"
      [SQL.Only (_ :: Int)] <- SQL.query_ conn "SELECT 1"
      return conn
    death = SQL.close

findJob :: Cn -> Value -> IO (Maybe Job)
findJob pool needle = do
  Pool.withResource pool $ \conn -> do
    rows :: [(Int, Value, Int, UTCTime)] <-
      SQL.query
        conn
        "SELECT id, name, status, dt FROM jobs WHERE name = ?"
        (SQL.Only needle)
    return . listToMaybe $
      [Job key name (toEnum status) dt | (key, name, status, dt) <- rows]

insertJob :: Cn -> Value -> IO Job
insertJob pool name = do
  Pool.withResource pool $ \conn -> do
    dt <- getCurrentTime
    [SQL.Only key] <-
      SQL.query
        conn
        "INSERT INTO jobs (name, status, dt) VALUES (?, ?, ?) RETURNING id"
        (name, fromEnum status, dt)
    return (Job key name status dt)
  where
    status = Alive

gimmeAJob :: Cn -> IO (Maybe Job)
gimmeAJob pool = do
  Pool.withResource pool $ \conn -> do
    rows <-
      SQL.query_
        conn
        "SELECT id, name, status, dt FROM jobs ORDER BY dt LIMIT 1"
    return . listToMaybe $
      [Job key name (toEnum status) dt | (key, name, status, dt) <- rows]
