{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (scotty, middleware)
import Data.Default
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import Network.Wai.Middleware.Static
import Data.Boolean

import Web.KansasComet
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.Types
import Language.Sunroof.Canvas
import Language.Sunroof.Browser

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

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
    registerEvents doc "body" mempty
    
    theCookie <- sync doc $ document <!> cookie
    putStrLn $ "Cookie:     " ++ show theCookie
    
    theTitle <- sync doc $ document <!> title
    putStrLn $ "Title:      " ++ show theTitle
    
    theReferrer <- sync doc $ document <!> referrer
    putStrLn $ "Referrer:   " ++ show theReferrer
    
    theUrl <- sync doc $ document <!> url
    putStrLn $ "URL:        " ++ show theUrl
    
    theUserAgent <- sync doc $ object "navigator" <!> attribute "userAgent"
    putStrLn $ "User Agent: " ++ show theUserAgent
    
    theWidth <- sync doc $ screen <!> attribute "width" :: IO JSNumber
    theHeight <- sync doc $ screen <!> attribute "height" :: IO JSNumber
    putStrLn $ "Screen Size:   " ++ show theWidth ++ " x " ++ show theHeight
    
    theOuterWidth <- sync doc $ window <!> attribute "outerWidth" :: IO JSNumber
    theOuterHeight <- sync doc $ window <!> attribute "outerHeight" :: IO JSNumber
    putStrLn $ "Window Size:   " ++ show theOuterWidth ++ " x " ++ show theOuterHeight
    
    theInnerWidth <- sync doc $ window <!> attribute "innerWidth" :: IO JSNumber
    theInnerHeight <- sync doc $ window <!> attribute "innerHeight" :: IO JSNumber
    putStrLn $ "Viewport Size: " ++ show theInnerWidth ++ " x " ++ show theInnerHeight
      
    async doc $ do
      println "Cookie" theCookie
      println "Title" theTitle
      println "Referrer" theReferrer
      println "URL" theUrl
      println "User Agent" theUserAgent
      println "Screen Size" $ cast theWidth <> " x " <> cast theHeight
      println "Window Size" $ cast theOuterWidth <> " x " <> cast theOuterHeight
      println "Viewport Size" $ cast theInnerWidth <> " x " <> cast theInnerHeight

screen :: JSObject
screen = object "screen"

jQuery :: JSString -> JS JSObject
jQuery nm = call "$" <$> with [cast nm]

append :: JSString -> Action JSObject ()
append x = method "append" [cast x]

println :: JSString -> JSString -> JS ()
println name val = do
  let val' = ifB (val ==* "") "&lt;EMPTY&gt;" val
  jQuery "#output" <*> append ("<dt>" <> name <> "</dt>")
  jQuery "#output" <*> append ("<dd>" <> val' <> "</dd>")




