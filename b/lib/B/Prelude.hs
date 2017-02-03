module B.Prelude
  ( module X
  , Text.Text
  , put
  , log
  ) where

import BasePrelude as X hiding (lazy, (&), uncons, index, log)
import Control.Lens as X
import Data.Text.Lens as X
import Data.Time as X

import qualified Data.Text as Text
import qualified System.IO as IO

put :: String -> IO ()
put = putStrLn

log :: String -> IO ()
log = IO.hPutStrLn IO.stderr
