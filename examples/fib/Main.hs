{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.Default ( Default(..) )
import Data.Semigroup ( (<>) )
import Control.Monad ( liftM2 )
import Data.Boolean

import Web.KansasComet ( registerEvents, event, (<&>), (.=) )
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.KansasComet
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser ( alert )
import Language.Sunroof.JS.JQuery

main2 :: IO ()
main2 = do
    staticCompiler def "main" prog >>= writeFile "main.js"

main :: IO ()
main = sunroofServer (def { cometResourceBaseDir = ".." }) $ \doc -> do
  registerEvents (cometDocument doc) "body" (slide <> click)
  async doc prog

prog :: JSB ()
prog = do

      ch <- newChan

      jq "body" >>= on "click" ".click" (\ () -> do
                the_id :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                o <- new "Object" ()
                o # "id" := the_id
                ch # writeChan o)
      jq "body" >>= on "slide" ".slide" (\ (a :: JSObject, aux :: JSObject) -> do
                the_id :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                o <- new "Object" ()
                o # "id" := the_id
                o # "value" := (aux ! "value" :: JSString)
                ch # writeChan o)

      obj <- new "Object" ()
      obj # attr "model" := (0 :: JSNumber)

      -- This is using the imperative update to enable the
      let slider :: JSNumber -> JSObject -> JSB JSObject
          slider nm = invoke "slider"  ("value" :: JSString, nm)

          update :: String -> JSNumber -> JSNumber -> JSNumber -> JSB ()
          update nm val mn mx =
              ifB ((val <=* mx) &&* (val >=* mn))
                  (obj # attr nm := val)
                  (return ())

          switchB _   []         def = def
          switchB tag ((a,b):xs) def = ifB (tag ==* a) b (switchB tag xs def)

      fib <- function $ fixJS $ \ fib (n :: JSNumber) -> do
          ifB (n <* 2)
              (return (1 :: JSNumber))
              (liftM2 (+) (fib (n - 1)) (fib (n - 2)))

      loopJS () $ \() -> do
          res <- ch # readChan
--          res <- wait "body" (slide <> click)
          model <- evaluate (obj ! "model") :: JSB JSNumber

          switchB (res ! "id" :: JSString)
                  [ ("slider", update "model" (res ! "value") 0 25)
                  , ("up"    , update "model" (model + 1)     0 25)
                  , ("down"  , update "model" (model - 1)     0 25)
                  , ("reset" , update "model" 0               0 25)
                  ] $ return ()

          model <- evaluate (obj ! "model") :: JSB JSNumber
          jQuery "#slider"  >>= slider (cast model)
          liftJS $ do
                jQuery "#fib-out" >>= html ("fib " <> cast model <> "...")
                res <- apply fib model
                jQuery "#fib-out" >>= html ("fib " <> cast model <> " = " <> cast res)
                return ()

      return ()

default(JSNumber, JSString, String)

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Event = Slide String Int
           | Click String Int Int
    deriving (Show)

slide = event "slide" Slide
            <&> "id"      .= "$(widget).attr('id')"
            <&> "value"   .= "aux.value"

click = event "click" Click
            <&> "id"      .= "$(widget).attr('id')"
            <&> "pageX"   .=  "event.pageX"
            <&> "pageY"   .=  "event.pageY"

recfunction :: (JSArgument a, Sunroof b) => ((a -> JSA b) -> (a -> JSA b)) -> JS t (JSFunction a b)
recfunction fn = do
        obj <- new "Object" ()
        f <- function $ fn (\ n -> obj # invoke "rec" n)
        obj # attr "rec" := f
        return f

-- To be moved to a more general place

fixJS :: (JSArgument a, Sunroof b) => ((a -> JSA b) -> (a -> JSA b)) -> a -> JSA b
fixJS f a = do
        ref <- newJSRef (cast nullJS)
        fn <- function $ \ a' -> do
                        fn' <- readJSRef ref
                        f (apply fn') a'
        writeJSRef ref fn
        apply fn a


