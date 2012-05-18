{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- Example of using tractor

module Main where

import Data.Aeson as A
import Data.Aeson.Types as AP
import Web.Scotty
import Web.Tractor as T
import Data.Default
import Data.Monoid
import Control.Monad
import qualified Control.Applicative as App
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as Text

-- import Web.Sunroof
import Language.Sunroof
import Language.Sunroof.Class

opts :: T.Options
opts = def { prefix = "/example", verbose = 2 }

main = do
        -- build the scotty dispatch app
        scotty 3000 $ do
                -- provide some static pages, include jquery
                -- This is scotty code
                get "/" $ file $ "index.html"
                sequence_ [ get (literal ("/" ++ nm)) $ file $  nm
                          | nm <- ["jquery.js","jquery-json.js"]
                          ]
                j_tractor <- liftIO jTractorStatic
                get "/jquery-tractor.js" $ file $ j_tractor

                -- connect /example to the following web_app
                connect opts sunroof_app

sunroof_app :: Document -> IO ()
sunroof_app doc = do
        print "sunroof_app"

        sendS doc $ loop $ do
                event <- waitForS "mousemove"
                c <- getContext "my-canvas"
                let (x,y) = (event ! "x",event ! "y")
                c <$> beginPath()
                c <$> arc(x, y, 20, 0, 2 * pi, false)
                c <$> fillStyle := "#8ED6FF"
                c <$> fill()

showJ :: (Sunroof a) => a -> JSString
showJ = cast

sendS :: (Sunroof a) => Document -> JSM a -> IO ()
sendS doc jsm = do
        let (txt,ty) = toC $ compileJS jsm
        print ("TXT:",txt)
        print ("TY:",ty)
        send doc (Text.pack $ "(" ++ txt ++ ")(function(k){})")
        return ()

-------------------------------------------------------------

waitForS :: JSString -> JSM JSObject
waitForS event =
        jsSelect $ JSS_Call "waitForS" [cast event] Value Continue
-------------------------------------------------------------


getContext :: JSString -> JSM JSObject
getContext nm = jsSelect $ JSS_Call "getContext" [cast nm] Value Direct

arc :: (JSNumber,JSNumber,JSNumber,JSNumber,JSNumber,JSBool) -> JSS ()
arc (a1,a2,a3,a4,a5,a6) = JSS_Call "arc" [cast a1,cast a2,cast a3,cast a4,cast a5,cast a6] Unit Direct :: JSS ()

beginPath :: () -> JSS ()
beginPath () = JSS_Call "beginPath" [] Unit Direct
{-
bezierCurveTo :: (JSNumber,JSNumber,JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
bezierCurveTo = Command . BezierCurveTo

clearRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
clearRect = Command . ClearRect

closePath :: () -> JSS ()
closePath () = Command ClosePath
-}
fill :: () -> JSS ()
fill () = JSS_Call "fill" [] Unit Direct

{-
fillRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
fillRect = Command . FillRect
-}
fillStyle :: JSF JSString
fillStyle = field "fillStyle"
{-
fillText :: (String,JSNumber,JSNumber) -> JSS ()
fillText = Command . FillText

font :: String -> JSS ()
font = Command . Font

globalAlpha :: JSNumber -> JSS ()
globalAlpha = Command . GlobalAlpha

lineCap :: String -> JSS ()
lineCap = Command . LineCap

lineJoin :: String -> JSS ()
lineJoin = Command . LineJoin
-}
lineTo :: (JSNumber,JSNumber) -> JSS ()
lineTo (a1,a2) = JSS_Call "lineTo" [cast a1,cast a2] Unit Direct :: JSS ()

lineWidth :: JSF JSNumber
lineWidth  = field "lineWidth"
{-
miterLimit :: JSNumber -> JSS ()
miterLimit = Command . MiterLimit
-}
moveTo :: (JSNumber,JSNumber) -> JSS ()
moveTo (a1,a2) = JSS_Call "moveTo" [cast a1,cast a2] Unit Direct :: JSS ()
{-
foo :: JSInt -> JSS ()

Command . MoveTo

restore :: () -> JSS ()
restore () = Command Restore

rotate :: JSNumber -> JSS ()
rotate = Command . Rotate

scale :: (JSNumber,JSNumber) -> JSS ()
scale = Command . Scale

save :: () -> JSS ()
save () = Command Save
-}
stroke :: () -> JSS ()
stroke () = JSS_Call "stroke" [] Unit Direct :: JSS ()
{-
strokeRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
strokeRect = Command . StrokeRect

strokeText :: (String,JSNumber,JSNumber) -> JSS ()
strokeText = Command . StrokeText
-}
strokeStyle :: JSF JSString
strokeStyle = field "strokeStyle"
{-
textAlign :: String -> JSS ()
textAlign = Command . TextAlign

textBaseline :: String -> JSS ()
textBaseline = Command . TextBaseline

transform :: (JSNumber,JSNumber,JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
transform = Command . Transform

translate :: (JSNumber,JSNumber) -> JSS ()
translate = Command . Translate

-}
