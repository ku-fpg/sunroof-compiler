{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty (scotty, middleware)
import Data.Default
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import Control.Applicative
import Data.Active
import Network.Wai.Middleware.Static
import Data.Boolean

import Web.KansasComet
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.Types
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.JQuery
import Language.Sunroof.Active

main :: IO ()
main = sunroofServer (defaultServerOpts { cometResourceBaseDir = ".." }) $ \ doc -> do
        registerEvents (cometDocument doc) "body" click
        async doc (example doc)

example :: SunroofEngine -> JS ()
example doc = do
  canvas <- document # getElementById "canvas"
  c <- canvas # getContext "2d"
  -- Stuff
  (s,e,f) <- reifyActiveJS $ ((\ (t :: JSNumber) -> do
        c # clearRect (0,0) (canvas ! width, canvas ! height)
        c # beginPath
        c # moveTo (t * 100,150)
        c # lineTo (450,50)
        c # closePath
        c # stroke) <$> ui)

  n <- new
  n # "val" := (s :: JSNumber)
  loop <- function $ \ () -> do
                apply f (n ! "val")
                n # "val" := ((n ! "val") + 0.01 :: JSNumber)
                ifB ((n ! "val") >* e)
                    (window # clearInterval (n ! "callsign"))
                    (return ())
                return ()
  v <- window # setInterval loop 20
  n # "callsign" := v
  return ()


default(JSNumber, JSString, String)

data Event = Click String Int Int
    deriving (Show)

click = event "click" Click
            <&> "id"      .= "$(widget).attr('id')"
            <&> "pageX"   .=  "event.pageX"
            <&> "pageY"   .=  "event.pageY"
