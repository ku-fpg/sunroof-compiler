{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Main where

import Prelude hiding (mod, div)

import Data.Monoid
import Data.Boolean
import Data.Boolean.Numbers
import Data.Default

import Control.Monad
import Control.Monad.IO.Class

import Network.Wai.Middleware.Static
import Web.Scotty (scotty, middleware)
import Web.KansasComet
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.Types
import Language.Sunroof.Canvas
import Language.Sunroof.Browser hiding ( eval )

main :: IO ()
main = scotty 3000 $ do
    kcomet <- liftIO kCometPlugin

    let pol = only [("","index.html")
                   ,("js/kansas-comet.js", kcomet)]
              <|> ((hasPrefix "css/" <|> hasPrefix "js/") >-> addBase "..")

    middleware $ staticPolicy pol

    KC.connect opts web_app

opts :: KC.Options
opts = def { prefix = "/example", verbose = 0 }

default(JSNumber, JSString, String)

type instance BooleanOf () = JSBool

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = async doc clockJS

-- TODO: Would be neat to create JS functions with more then one parameter.
clockJS :: JS ()
clockJS = do
  -- Renders a single line (with number) of the clock face.
  renderClockFaceLine <- function $ \c u n -> do
    c <$> save
    -- Draw one of the indicator lines
    c <$> beginPath
    c <$> moveTo (0, -u * 1.0)
    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (c <$> lineTo (0, -u * 0.8)) -- Minute line
        (c <$> lineTo (0, -u * 0.9)) -- Hour line
    ifB (n `mod` 15 ==* (0 :: JSNumber))
        (c <$> setLineWidth 8) -- Quarter line
        (c <$> setLineWidth 3) -- Non-Quarter line
    c <$> stroke
    c <$> closePath
    -- Draw of the hour numbers
    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (do
          c <$> translate (-u * 0.75, 0)
          c <$> rotate (-2 * pi / 4)
          c <$> fillText (cast $ n `div` 5) (0, 0)
        ) (return ())
    c <$> restore
  
  renderClockPointer <- function $ \c u angle width len -> do
    c <$> save
    c <$> setLineCap "round"
    c <$> rotate angle
    c <$> setLineWidth width
    c <$> beginPath
    c <$> moveTo (0, u * 0.1)
    c <$> lineTo (0, -u * len)
    c <$> stroke
    c <$> closePath
    c <$> restore
  
  -- Renders the clocks pointers for hours, minutes and seconds.
  renderClockPointers <- function $ \c u -> do
    (h, m, s) <- currentTime
    c <$> save
    -- Hour pointer
    renderClockPointer <$> with 
      [ cast c
      , cast (u :: JSNumber)
      , cast $ (2 * pi / 12 :: JSNumber) * ((h `mod` 12) + (m `mod` 60) / 60))
      , cast (15 :: JSNumber)
      , cast (0.4 :: JSNumber) ]
    -- Minute pointer
    renderClockPointer <$> with 
      [ cast c
      , cast u
      , cast $ (2 * pi / 60 :: JSNumber) * ((m `mod` 60) + (s `mod` 60) / 60))
      , cast (10 :: JSNumber)
      , cast (0.7 :: JSNumber) ]
    -- Second pointer
    c <$> setStrokeStyle "red"
    renderClockPointer <$> with 
      [ cast c
      , cast u
      , cast $ (2 * pi / 60 :: JSNumber) * (s `mod` 60)
      , cast (4 :: JSNumber)
      , cast (0.9 :: JSNumber) ]
    -- Restore everything
    c <$> restore
  
  -- Renders the complete face of the clock, without pointers.
  renderClockFace <- function $ \c u -> do
    c <$> save
    c <$> rotate (2 * pi / 4) -- 0 degrees is at the top
    -- Draw all hour lines.
    -- TODO: This repeats the call 60 times. Would be neat to have loops.
    sequence_ $ (flip fmap) (fmap fromInteger [1..60] :: [JSNumber]) $ \n -> do
      c <$> rotate (2 * pi / 60)
      renderClockFaceLine <$> with [cast c, cast (u :: JSNumber), cast n]
    c <$> restore -- Undo all the rotation.
    
  -- Renders the complete clock.
  renderClock <- function $ \() -> do
    u <- clockUnit
    (w,h) <- canvasSize
    c <- context
    -- Basic setup
    c <$> save
    c <$> setFillStyle "black"
    c <$> setStrokeStyle "black"
    c <$> setLineCap "round"
    c <$> setTextAlign "center"
    c <$> setFont ((cast $ u * 0.1) <> "px serif")
    c <$> setTextBaseline "top"
    c <$> clearRect (0,0) (w,h)
    c <$> translate (w / 2, h / 2)
    -- Draw all hour lines.
    renderClockFace <$> with [cast c, cast u]
    -- Draw the clock pointers
    renderClockPointers <$> with [cast c, cast u]
    c <$> restore
    return ()
  
  renderClock <$> with [cast ()]
  window <$> setInterval renderClock 1000
  
  return ()

-- TODO: Move this into a new JQuery module of sunroof.
jQuery :: JSString -> JS JSObject
jQuery nm = call "$" <$> with [cast nm]

canvas :: JS JSObject
canvas = document <$> getElementById "canvas"

context :: JS JSObject
context = canvas <*> getContext "2d"

clockUnit :: JS JSNumber
clockUnit = do
  (w, h) <- canvasSize
  return $ (maxB w h) / 2

canvasSize :: JS (JSNumber, JSNumber)
canvasSize = do
  c <- jQuery "#canvas"
  w <- c <$> method "innerWidth" []
  h <- c <$> method "innerHeight" []
  return (w, h)

currentTime :: JS (JSNumber, JSNumber, JSNumber)
currentTime = do
  date <- eval $ object "new Date()"
  h <- date <$> method "getHours" []
  m <- date <$> method "getMinutes" []
  s <- date <$> method "getSeconds" []
  return (h, m, s)
