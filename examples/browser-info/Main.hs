{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default ( Default(..) )
import Data.Semigroup ( (<>) )
import Data.Boolean
import Data.Maybe ( fromJust )
import Data.String ( IsString(..) )

import Control.Monad ( liftM2 )

import Language.Sunroof
import Language.Sunroof.Server
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.JQuery

default(JSNumber, JSString, String)

main :: IO ()
main = sunroofServer (def { cometResourceBaseDir = ".." }) $ \doc -> do

  theCookie <- syncJS doc $ evaluate $ document ! cookie
  putStrLn $ "Cookie:     " ++ show theCookie

  theTitle <- syncJS doc $ evaluate $ document ! title
  putStrLn $ "Title:      " ++ show theTitle

  theReferrer <- syncJS doc $ evaluate $ document ! referrer
  putStrLn $ "Referrer:   " ++ show theReferrer

  theUrl <- syncJS doc $ evaluate $ document ! url
  putStrLn $ "URL:        " ++ show theUrl

  theUserAgent <- syncJS doc $ evaluate $ object "navigator" ! (attr "userAgent" :: JSSelector JSString)
  putStrLn $ "User Agent: " ++ show theUserAgent

  theWidth  <- syncJS doc
             $ evaluate $ screen ! (attr "width" :: JSSelector JSNumber)
  theHeight <- syncJS doc
             $ evaluate $ screen ! (attr "height" :: JSSelector JSNumber)
  putStrLn $ "Screen Size:   " ++ show theWidth ++ " x " ++ show theHeight

  theOuterWidth  <- syncJS doc
                  $ evaluate $ window ! (attr "outerWidth" :: JSSelector JSNumber)
  theOuterHeight <- syncJS doc
                  $ evaluate $ window ! (attr "outerHeight" :: JSSelector JSNumber)
  putStrLn $ "Window Size:   " ++ show theOuterWidth ++ " x " ++ show theOuterHeight

  theInnerWidth  <- syncJS doc
                  $ evaluate $ window ! (attr "innerWidth" :: JSSelector JSNumber)
  theInnerHeight <- syncJS doc
                  $ evaluate $ window ! (attr "innerHeight" :: JSSelector JSNumber)
  putStrLn $ "Viewport Size: " ++ show theInnerWidth ++ " x " ++ show theInnerHeight

  asyncJS doc $ do
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




