{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Default
import Data.Boolean

import Web.KansasComet

import Language.Sunroof
import Language.Sunroof.KansasComet
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.JQuery

-- This is a transcription of the http://www.html5canvastutorials.com/ demos/tutorials.

main :: IO ()
main = sunroofServer (def { cometResourceBaseDir = ".." }) $ \ doc -> do
  registerEvents (cometDocument doc) "body" click
  sequence_ $ map (\ex -> whenEvent (cometDocument doc) "body" click 
                        $ async doc $ onClick $ ex)
                  (cycle examples)
  --whenEvent doc "body" click
  --  $ sync doc $ onClick $ examples !! 0
  --sequence_ $ map (sync doc) $ map waitForClick $ examples
  --return ()

default(JSNumber, JSString, String)

whenEvent :: Document -> Scope -> Template event -> IO a -> IO a
whenEvent doc scope event m = do
  e <- waitForEvent doc scope event
  case e of
    Nothing -> whenEvent doc scope event m
    Just e  -> m

onClick :: (JSObject -> JSCanvas -> JSA (), JSString) -> JSA()
onClick (js, msg) = do
  canvas <- document # getElementById "canvas"
  c <- canvas # getContext "2d"
  c # clearRect (0,0) (canvas ! width, canvas ! height)
  js canvas c
  message canvas c msg
    {-
  wait "body" click $ \ res -> do
    ifB ((res ! "id" :: JSString) ==* "canvas")
      (do )
      (return ())
      waitForClick [] = return ()-}

data Event = Click String Int Int
    deriving (Show)

click = event "click" Click
            <&> "id"      .= "$(widget).attr('id')"
            <&> "pageX"   .=  "event.pageX"
            <&> "pageY"   .=  "event.pageY"

examples :: [(JSObject -> JSCanvas -> JSA (), JSString)]
examples =
  [ (example_1_2_1,"1.2.1 Line")
  , (example_1_2_2,"1.2.2 Line Width")
  , (example_1_2_3,"1.2.3 Line Color")
  , (example_1_2_4,"1.2.4 Line Cap")
  , (example_1_3_1,"1.3.1 Arc")
  , (example_1_5_4,"1.5.4 Circle")
  , (example_1_8_1,"1.8.1 Text Font & Size")
  , (example_1_8_2,"1.8.2 Text Color")
  , (example_1_8_3,"1.8.3 Text Stroke")
  , (example_1_8_4,"1.8.4 Text Align")
  , (example_1_8_5,"1.8.5 Text Baseline")
  ]

example_1_2_1 :: JSObject -> JSCanvas -> JSA ()
example_1_2_1 canvas c = do
  c # beginPath
  c # moveTo (100,150)
  c # lineTo (450,50)
  c # closePath
  c # stroke

example_1_2_2 :: JSObject -> JSCanvas -> JSA ()
example_1_2_2 canvas c = do
  c # beginPath
  c # moveTo (100,150)
  c # lineTo (450,50)
  c # closePath
  c # setLineWidth 15
  c # stroke

example_1_2_3 :: JSObject -> JSCanvas -> JSA ()
example_1_2_3 canvas c = do
  c # beginPath
  c # moveTo (100,150)
  c # lineTo (450,50)
  c # closePath
  c # setLineWidth 5
  c # setStrokeStyle "#ff0000"
  c # stroke

example_1_2_4 :: JSObject -> JSCanvas -> JSA ()
example_1_2_4 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  sequence_
    [ do c # beginPath
         c # moveTo (200, h / 2 + n)
         c # lineTo (w - 200, h / 2 + n)
         c # closePath
         c # setLineWidth 20
         c # setStrokeStyle "#0000ff"
         c # setLineCap cap
         c # stroke
    | (cap,n) <- zip ["butt","round","square"] [-50,0,50]
    ]

example_1_3_1 :: JSObject -> JSCanvas -> JSA ()
example_1_3_1 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let centerX = w / 2;
  let centerY = h / 2;
  let radius = 75;
  let startingAngle = 1.1 * pi
  let endingAngle = 1.9 * pi
  let counterclockwise = false
  c # beginPath
  c # arc' (centerX, centerY) radius (startingAngle, endingAngle) counterclockwise
  c # closePath
  c # setLineWidth 15
  c # setStrokeStyle "black"
  c # stroke

example_1_5_4 :: JSObject -> JSCanvas -> JSA ()
example_1_5_4 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let centerX = w / 2
  let centerY = h / 2
  let radius = 70
  c # beginPath
  c # arc' (centerX, centerY) radius (0, 2 * pi) false
  c # setFillStyle "#8ED6FF"
  c # fill
  c # setLineWidth  5
  c # setStrokeStyle "black"
  c # stroke

example_1_8_1 :: JSObject -> JSCanvas -> JSA ()
example_1_8_1 canvas c = do
  c # setFont "40pt Calibri"
  c # fillText "Hello World!" (150, 100)

example_1_8_2 :: JSObject -> JSCanvas -> JSA ()
example_1_8_2 canvas c = do
  c # setFont "40pt Calibri"
  c # setFillStyle "#0000ff"
  c # fillText "Hello World!" (150, 100)

example_1_8_3 :: JSObject -> JSCanvas -> JSA ()
example_1_8_3 canvas c = do
  c # setFont "60pt Calibri"
  c # setLineWidth 3
  c # setStrokeStyle "blue"
  c # strokeText "Hello World!" (80, 110)

example_1_8_4 :: JSObject -> JSCanvas -> JSA ()
example_1_8_4 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let x = w / 2
  let y = h / 2
  -- Draw alignment line
  c # setStrokeStyle "red"
  c # beginPath
  c # moveTo (x, 0)
  c # lineTo (x, h)
  c # closePath
  c # setLineWidth 1
  c # stroke
  -- Draw text
  c # setFont "30px Calibri"
  c # setTextBaseline "top"
  c # setFillStyle "blue"
  -- Function to draw baseline identifier on its baseline.
  let textFun :: JSString -> JSNumber -> JSA JSNumber
      textFun al offset = do
        c # setTextAlign al
        c # fillText al (x, offset)
        return $ offset + 30
  -- Line the different identifiers up after each other.
  passFold_ 0 $ map textFun ["center", "end", "left", "right", "start"]

example_1_8_5 :: JSObject -> JSCanvas -> JSA ()
example_1_8_5 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let y = h / 2
  -- Draw baseline
  c # setStrokeStyle "red"
  c # beginPath
  c # moveTo (0, y)
  c # lineTo (w, y)
  c # closePath
  c # setLineWidth 1
  c # stroke
  -- Draw text
  c # setFont "15pt Calibri"
  c # setTextAlign "left"
  c # setFillStyle "blue"
  -- Function to draw baseline identifier on its baseline.
  let textFun :: JSString -> JSNumber -> JSA JSNumber
      textFun bl offset = do
        c # setTextBaseline bl
        c # fillText bl (offset, y)
        tm <- c # measureText bl
        return $ offset + (tm ! width)
  -- Line the different identifiers up after each other.
  passFold_ 0 $ map textFun [ "alphabetic"
                            , "top"
                            , "hanging"
                            , "middle"
                            , "ideographic"
                            , "bottom"]

passFold :: (Monad m) => a -> [a -> m a] -> m a
passFold e (k : ks) = k e >>= \e' -> passFold e' ks
passFold e [] = return e

passFold_ :: (Monad m) => a -> [a -> m a] -> m ()
passFold_ e l = passFold e l >> return ()

message :: JSObject -> JSCanvas -> JSString -> JSA ()
message canvas c msg = do
  c # save
  c # setFont "30pt Calibri"
  c # setTextAlign "left"
  c # setTextBaseline "alphabetic"
  c # setFillStyle "#8090a0"
  c # fillText msg (10, (canvas ! height) - 10)
  c # restore
