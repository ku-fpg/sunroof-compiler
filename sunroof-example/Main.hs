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

import Web.Sunroof

main = main_sunroof

main_tractor = do

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
                connect opts web_app

opts :: T.Options
opts = def { prefix = "/example", verbose = 2 }

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
        print "web_app"

        register doc "click" $ Text.pack $ concat
                [ " return { pageX : event.pageX"
                , "        , pageY : event.pageY"
                , "        , id    : $(widget).attr('id')"
                , "        };"
                ]

        forkIO $ forever $ do
                res <- waitFor doc "click"
                res <- query doc (Text.pack "return { wrapped : $('#fib-in').attr('value') };")
                let Success (Wrapped a) :: Result (Wrapped String) = parse parseJSON res
                print a
                case reads a of
                  [(v :: Int,"")] -> do
                        send doc (Text.pack $ "$('#fib-out').html('&#171;&#8226;&#187;')")
                        send doc (Text.pack $ "$('#fib-out').text('" ++ show (fib v) ++ "')")
                  _ ->  send doc (Text.pack $ "$('#fib-out').text('...')")

--                let Success b :: Result String = parse parseJSON a
--                print b
                print res
        return ()

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Wrapped a = Wrapped a
        deriving Show

instance FromJSON a => FromJSON (Wrapped a) where
   parseJSON (Object v) = Wrapped    App.<$>
                          (v .: "wrapped")
   parseJSON _          = mzero


main_sunroof = do

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

        send doc "alert('x');"
        sendS doc $ do
                alert "ABC"
                c <- getContext "my-canvas"
                c <$> moveTo(50,50)
                c <$> lineTo(200,100)
                c <$> lineWidth := 10
                c <$> strokeStyle := "red"
                c <$> stroke()


showJ :: (Sunroof a) => a -> JSString
showJ = cast

sendS :: (Sunroof a) => Document -> JSM a -> IO ()
sendS doc jsm = do
        let ((txt,ty),_) = runCompM (compileC jsm) 0
        print ("TXT:",txt)
        print ("TY:",ty)
        send doc (Text.pack $ "(" ++ txt ++ ")(function(k){})")
        return ()

-------------------------------------------------------------

getContext :: JSString -> JSM JSObject
getContext nm = JS_Select $ JSS_Call "getContext" [cast nm] Value Direct

{-

arc :: (JSNumber,JSNumber,JSNumber,JSNumber,JSNumber,Bool) -> JSS ()
arc = Command . Arc

beginPath :: () -> JSS ()
beginPath () = Command BeginPath

bezierCurveTo :: (JSNumber,JSNumber,JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
bezierCurveTo = Command . BezierCurveTo

clearRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
clearRect = Command . ClearRect

closePath :: () -> JSS ()
closePath () = Command ClosePath

fill :: () -> JSS ()
fill () = Command Fill

fillRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> JSS ()
fillRect = Command . FillRect

fillStyle :: String -> JSS ()
fillStyle = Command . FillStyle

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
