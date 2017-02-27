{-# LANGUAGE OverloadedStrings #-}

module B.Views where

import B.Prelude
import Lucid

layout :: Html () -> Html ()
layout inner = do
  doctype_
  html_ $ do
    head_ $ do
      link_ [rel_ "stylesheet", href_ "/f/static/css.css"]
      meta_ [charset_ "utf-8"]
    body_ inner

channelView :: String -> Html ()
channelView channel = layout (p_ [] (fromString channel))

home :: [String] -> Html ()
home dirs =
  layout . aside_ $ do
    h2_ "channels"
    ul_ [] (mapM_ leaf dirs)
  where
    leaf :: String -> Html ()
    leaf dir =
      li_ $
      a_
        [href_ (view packed $ printf "/channel/%s" dir)]
        (fromString $ printf "#%s" dir)
