{-# LANGUAGE OverloadedStrings #-}

module B.DB where

import B.Prelude
import qualified Database.PostgreSQL.Simple as SQL

data JobStatus
  = Alive
  | Done
  | Failed
  deriving (Show, Enum)

data Job =
  Job Int
      String
      JobStatus
      UTCTime
  deriving (Show)

type Cn = SQL.Connection

connect :: IO Cn
connect = do
  conn <-
    SQL.connectPostgreSQL
      "host='127.0.0.1' port=5432 dbname='loiter' user='loiter' password='loiter'"
  [SQL.Only (_ :: Int)] <- SQL.query_ conn "SELECT 1"
  return conn

findJob :: Cn -> String -> IO [Job]
findJob conn needle = do
  rows :: [(Int, String, Int, UTCTime)] <-
    SQL.query
      conn
      "SELECT id, name, status, dt FROM jobs WHERE name = ?"
      (SQL.Only needle)
  return [Job key name (toEnum status) dt | (key, name, status, dt) <- rows]

insertJob :: Cn -> String -> JobStatus -> IO Job
insertJob conn name status = do
  dt <- getCurrentTime
  [SQL.Only key] <-
    SQL.query
      conn
      "INSERT INTO jobs (name, status, dt) VALUES (?, ?, ?) RETURNING id"
      (name, fromEnum status, dt)
  return (Job key name status dt)
