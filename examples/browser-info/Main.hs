{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default
import Data.Monoid
import Data.Boolean
import Data.Maybe ( fromJust )
import Data.String

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
main = sunroofServer (defaultServerOpts { cometResourceBaseDir = ".." }) $ \doc -> do
  registerEvents (cometDocument doc) "body" mempty

  theCookie <- sync doc $ document <!> cookie
  putStrLn $ "Cookie:     " ++ show theCookie

  theTitle <- sync doc $ document <!> title
  putStrLn $ "Title:      " ++ show theTitle

  theReferrer <- sync doc $ document <!> referrer
  putStrLn $ "Referrer:   " ++ show theReferrer

  theUrl <- sync doc $ document <!> url
  putStrLn $ "URL:        " ++ show theUrl

  theUserAgent <- sync doc $ object "navigator" <!> (attribute "userAgent" :: JSSelector JSString)
  putStrLn $ "User Agent: " ++ show theUserAgent

  theWidth  <- sync doc
             $ screen <!> (attribute "width" :: JSSelector JSNumber)
  theHeight <- sync doc
             $ screen <!> (attribute "height" :: JSSelector JSNumber)
  putStrLn $ "Screen Size:   " ++ show theWidth ++ " x " ++ show theHeight

  theOuterWidth  <- sync doc
                  $ window <!> (attribute "outerWidth" :: JSSelector JSNumber)
  theOuterHeight <- sync doc
                  $ window <!> (attribute "outerHeight" :: JSSelector JSNumber)
  putStrLn $ "Window Size:   " ++ show theOuterWidth ++ " x " ++ show theOuterHeight

  theInnerWidth  <- sync doc
                  $ window <!> (attribute "innerWidth" :: JSSelector JSNumber)
  theInnerHeight <- sync doc
                  $ window <!> (attribute "innerHeight" :: JSSelector JSNumber)
  putStrLn $ "Viewport Size: " ++ show theInnerWidth ++ " x " ++ show theInnerHeight

  async doc $ do
    println "Cookie" theCookie
    println "Title" theTitle
    println "Referrer" theReferrer
    println "URL" theUrl
    println "User Agent" theUserAgent
    println "Screen Size" $ show theWidth <> " x " <> show theHeight
    println "Window Size" $ show theOuterWidth <> " x " <> show theOuterHeight
    println "Viewport Size" $ show theInnerWidth <> " x " <> show theInnerHeight

default(JSNumber, JSString, String)

screen :: JSObject
screen = object "screen"

jQuery :: JSString -> JS JSObject
jQuery nm = call "$" `apply` nm

append :: JSString -> Action JSObject ()
append x = method "append" x

println :: (Show a, Eq a) => JSString -> a -> JS ()
println name val = do
  let valStr = show val
  let val' = if valStr == "" then "&lt;EMPTY&gt;" else valStr
  jQuery "#output" >>= append ("<dt>" <> name <> "</dt>")
  jQuery "#output" >>= append ("<dd>" <> js val' <> "</dd>")




