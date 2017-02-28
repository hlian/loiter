module B.Prelude.Globals where

import Data.Text (Text)

import BasePrelude
import Control.Lens
import Data.Text.Strict.Lens

{-# NOINLINE mode #-}
mode :: String
mode = unsafePerformIO (getEnv "mode")

{-# NOINLINE port #-}
port :: Int
port = read . unsafePerformIO $ getEnv "port"

{-# NOINLINE dir #-}
dir :: Text
dir = view packed . unsafePerformIO $ getEnv "dir"
