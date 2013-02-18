{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default
import Data.Monoid
import Data.Boolean
import Data.Maybe ( fromJust )

import Control.Monad
import Control.Monad.IO.Class

import Network.Wai.Middleware.Static
import Web.Scotty (scotty, middleware)
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

    theCookie <- fmap fromJust $ sync doc $ document <!> cookie
    putStrLn $ "Cookie:     " ++ show theCookie

    theTitle <- fmap fromJust $ sync doc $ document <!> title
    putStrLn $ "Title:      " ++ show theTitle

    theReferrer <- fmap fromJust $ sync doc $ document <!> referrer
    putStrLn $ "Referrer:   " ++ show theReferrer

    theUrl <- fmap fromJust $ sync doc $ document <!> url
    putStrLn $ "URL:        " ++ show theUrl

    theUserAgent <- fmap fromJust $ sync doc $ object "navigator" <!> attribute "userAgent"
    putStrLn $ "User Agent: " ++ show theUserAgent

    theWidth  <- fmap fromJust
               $ sync doc
               $ screen <!> attribute "width" :: IO JSNumber
    theHeight <- fmap fromJust
               $ sync doc
               $ screen <!> attribute "height" :: IO JSNumber
    putStrLn $ "Screen Size:   " ++ show theWidth ++ " x " ++ show theHeight

    theOuterWidth  <- fmap fromJust
                    $ sync doc
                    $ window <!> attribute "outerWidth" :: IO JSNumber
    theOuterHeight <- fmap fromJust
                    $ sync doc
                    $ window <!> attribute "outerHeight" :: IO JSNumber
    putStrLn $ "Window Size:   " ++ show theOuterWidth ++ " x " ++ show theOuterHeight

    theInnerWidth  <- fmap fromJust
                    $ sync doc
                    $ window <!> attribute "innerWidth" :: IO JSNumber
    theInnerHeight <- fmap fromJust
                    $ sync doc
                    $ window <!> attribute "innerHeight" :: IO JSNumber
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
jQuery nm = call "$" <$> with nm

append :: JSString -> Action JSObject ()
append x = method "append" x

println :: JSString -> JSString -> JS ()
println name val = do
  let val' = ifB (val ==* "") "&lt;EMPTY&gt;" val
  jQuery "#output" <*> append ("<dt>" <> name <> "</dt>")
  jQuery "#output" <*> append ("<dd>" <> val' <> "</dd>")




