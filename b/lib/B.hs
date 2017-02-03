{-# LANGUAGE OverloadedStrings #-}

-- the b stands for "backend"
module B where

import qualified Lucid
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Static as Static
import qualified System.Directory as Directory

import B.Prelude
import B.Views

{-# NOINLINE mode #-}
mode :: String
mode = unsafePerformIO (getEnv "mode")

{-# NOINLINE port #-}
port :: Int
port = read . unsafePerformIO $ getEnv "port"

{-# NOINLINE dir #-}
dir :: String
dir = unsafePerformIO $ getEnv "dir"

homePage :: Wai.Application
homePage req respond = do
  everything <- Directory.listDirectory dir
  channels <-
    filterM
      (\entry -> Directory.doesDirectoryExist (printf "%s/%s" dir entry))
      everything
  let bytes = Lucid.renderBS (home channels)
  respond $ Wai.responseLBS HTTP.status200 [] bytes

channelPage :: Text -> Wai.Application
channelPage channel req respond = do
  everything <- Directory.listDirectory dir
  channels <-
    filterM
      (\entry -> Directory.doesDirectoryExist (printf "%s/%s" dir entry))
      everything
  let bytes = Lucid.renderBS (home channels)
  respond $ Wai.responseLBS HTTP.status200 [] bytes

serve :: Wai.Application
serve req respond = do
  case Wai.pathInfo req of
    [] -> homePage req respond
    ["channel", channel] -> channelPage channel req respond
    other -> do
      log (printf "serve: unrecognized path: %s" (show other))
      respond $ Wai.responseLBS HTTP.status400 [] ""

mainWithMode "serve" = do
  put (printf "mainWithMode: running on port %d" port)
  Warp.run port (Static.staticPolicy policy serve)
  where
    policy =
      Static.isNotAbsolute <> Static.noDots <> Static.hasPrefix "f/static"
mainWithMode hmm = do
  log (printf "mainWithMode: unrecognized mode %s" hmm)
  exitWith (ExitFailure 1)

main = mainWithMode mode
