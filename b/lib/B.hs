{-# LANGUAGE OverloadedStrings #-}

-- the b stands for "backend"
module B where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Lucid
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Static as Static
import qualified System.Directory as Directory

import qualified B.DB as DB
import B.Prelude
import B.Views

homePage :: Wai.Application
homePage req respond = do
  everything <- Directory.listDirectory (view unpacked dir)
  channels <-
    filterM
      (\entry -> Directory.doesDirectoryExist (printf "%s/%s" dir entry))
      everything
  let bytes = Lucid.renderBS (home channels)
  respond $ Wai.responseLBS HTTP.status200 [] bytes

channelPage :: DB.Cn -> Text -> Wai.Application
channelPage pool channel req respond = do
  let subdir = printf "%s/%s" dir channel
  let jobName =
        (DB.AbsorbChannel (DB.Book "friendsofjack") (DB.Channel channel))
  everything <- Directory.listDirectory (view unpacked dir)
  when (Text.isInfixOf "." channel) $ do error "no"
  itDoes <- Directory.doesDirectoryExist subdir
  unless itDoes $ do error "whoa buddy"
  jobMaybe <- DB.findJob pool (Aeson.toJSON jobName)
  job <-
    case jobMaybe of
      Nothing -> DB.insertJob pool (Aeson.toJSON jobName)
      Just job -> pure job
  let bytes = Lucid.renderBS (channelView channel job)
  respond $ Wai.responseLBS HTTP.status200 [] bytes

serve :: DB.Cn -> Wai.Application
serve pool req respond =
  case Wai.pathInfo req of
    [] -> homePage req respond
    ["channel", channel] -> channelPage pool channel req respond
    other -> do
      log (printf "serve: unrecognized path: %s" (show other))
      respond $ Wai.responseLBS HTTP.status400 [] ""

mainWithMode "serve" = do
  pool <- DB.connect
  put (printf "mainWithMode: running on port %d" port)
  Warp.run port (Static.staticPolicy policy (serve pool))
  where
    policy =
      Static.isNotAbsolute <> Static.noDots <> Static.hasPrefix "f/static"
mainWithMode "job" = do
  pool <- DB.connect
  jobMaybe <- DB.gimmeAJob pool
  case jobMaybe of
    Nothing -> error "no more work"
    Just (DB.Job _ value status time) ->
      case Aeson.parseEither Aeson.parseJSON value of
        Left e -> error e
        Right (DB.AbsorbChannel (DB.Book book) (DB.Channel chan)) ->
          print (book, chan)
mainWithMode hmm = do
  log (printf "mainWithMode: unrecognized mode %s" hmm)
  exitWith (ExitFailure 1)

main = do
  log (printf "main: globals: dir=\"%s\" mode=\"%s\"" dir mode)
  mainWithMode mode
