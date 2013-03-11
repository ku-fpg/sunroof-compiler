{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default ( Default(..) )
import Data.Semigroup ( (<>) )
import Data.Boolean
import Data.Maybe ( fromJust )
import Data.String ( IsString(..) )

import Control.Monad ( liftM2 )
import Control.Monad.IO.Class

import Web.KansasComet ( registerEvents, event, (<&>), (.=) )
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.KansasComet
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.JQuery

default(JSNumber, JSString, String)

main :: IO ()
main = sunroofServer (def { cometResourceBaseDir = ".." }) $ \doc -> do
--  registerEvents (cometDocument doc) "body" mempty

  theCookie <- sync doc $ evaluate $ document ! cookie
  putStrLn $ "Cookie:     " ++ show theCookie

  theTitle <- sync doc $ evaluate $ document ! title
  putStrLn $ "Title:      " ++ show theTitle

  theReferrer <- sync doc $ evaluate $ document ! referrer
  putStrLn $ "Referrer:   " ++ show theReferrer

  theUrl <- sync doc $ evaluate $ document ! url
  putStrLn $ "URL:        " ++ show theUrl

  theUserAgent <- sync doc $ evaluate $ object "navigator" ! (attr "userAgent" :: JSSelector JSString)
  putStrLn $ "User Agent: " ++ show theUserAgent

  theWidth  <- sync doc
             $ evaluate $ screen ! (attr "width" :: JSSelector JSNumber)
  theHeight <- sync doc
             $ evaluate $ screen ! (attr "height" :: JSSelector JSNumber)
  putStrLn $ "Screen Size:   " ++ show theWidth ++ " x " ++ show theHeight

  theOuterWidth  <- sync doc
                  $ evaluate $ window ! (attr "outerWidth" :: JSSelector JSNumber)
  theOuterHeight <- sync doc
                  $ evaluate $ window ! (attr "outerHeight" :: JSSelector JSNumber)
  putStrLn $ "Window Size:   " ++ show theOuterWidth ++ " x " ++ show theOuterHeight

  theInnerWidth  <- sync doc
                  $ evaluate $ window ! (attr "innerWidth" :: JSSelector JSNumber)
  theInnerHeight <- sync doc
                  $ evaluate $ window ! (attr "innerHeight" :: JSSelector JSNumber)
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

println :: (Show a, Eq a) => JSString -> a -> JSA ()
println name val = do
  let valStr = show val
  let val' = if valStr == "" then "&lt;EMPTY&gt;" else valStr
  jq "#output" >>= append (cast $ "<dt>" <> name <> "</dt>")
  jq "#output" >>= append (cast $ "<dd>" <> js val' <> "</dd>")




