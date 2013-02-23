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
import Language.Sunroof.Painting
import Data.VectorSpace (lerp)

main :: IO ()
main = sunroofServer (defaultServerOpts { cometResourceBaseDir = ".." }) $ \ doc -> do
        registerEvents (cometDocument doc) "body" click
        async doc (example doc)

example :: SunroofEngine -> JS ()
example doc = do
  canvas <- document # getElementById "canvas"
  c <- canvas # getContext "2d"

  let clear :: Painting = painting $ \ c -> c # clearRect (0,0) (canvas ! width, canvas ! height)

  let drawing :: Active JSTime Painting = aLineTo (100,100) (300,300)

  let prog = pure clear <> (drawing ->> aLineTo (300,100) (100,300))

  -- Stuff
  (s,e,f) <- reifyActiveJS $ fmap (draw c) $ prog

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


aLineTo :: (JSNumber,JSNumber) -> (JSNumber,JSNumber) -> Active JSTime Painting
aLineTo (x0,y0) (x1,y1) = clamp $ (\ (t::JSNumber) ->  painting $ \ c -> do
        c # beginPath
        c # moveTo (x0,y0)
        c # lineTo (lerp x0 x1 t,lerp y0 y1 t)
        c # closePath
        c # stroke) <$> ui

default(JSNumber, JSString, String)

data Event = Click String Int Int
    deriving (Show)

click = event "click" Click
            <&> "id"      .= "$(widget).attr('id')"
            <&> "pageX"   .=  "event.pageX"
            <&> "pageY"   .=  "event.pageY"
