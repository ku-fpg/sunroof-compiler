{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
import Language.Sunroof.Browser (alert)

main :: IO ()
main = defaultCometServer ".." $ \doc -> do
  registerEvents doc "body" (slide <> click)
  {- Playing with canvas...
  let getElementById :: JSString -> Action JSObject JSObject
      getElementById a = method "getElementById" [cast a]

  sync doc $ do
      canvas <- object "document" # getElementById "canvas"
      context <- canvas # getContext "2d"
      context # setFillStyle "rgba(0, 0, 255, .5)"
      context # fillRect (25, 25) (125, 125)

      alert (cast context)

      return ()
      -}
  async doc $ do
      obj <- new
      obj # attribute "model" := (0 :: JSNumber)

      -- This is using the imperative update to enable the
      let control :: () -> JS ()
          control () = obj ! "control" `apply` ()

          view :: () -> JS ()
          view () = obj ! "view" `apply` ()

          addMethod :: (JSArgument a, Sunroof a, Sunroof b) => String -> (a -> JS b) -> JS ()
          addMethod nm f = do n <- function f
                              obj # attribute nm := n

          jQuery :: JSString -> JS JSObject
          jQuery nm = call "$" `apply` nm

          slider :: JSNumber -> Action JSObject JSObject
          slider nm = method "slider"  ("value" :: JSString, nm)

          html :: JSString -> Action JSObject JSObject
          html nm = method "html"  nm

          fib :: JSNumber -> JS JSNumber
          fib n = obj ! "fib" `apply` n

          update :: String -> JSNumber -> JSNumber -> JSNumber -> JS ()
          update nm val mn mx =
              ifB ((val <=* mx) &&* (val >=* mn))
                  (obj # attribute nm := val)
                  (return ())

          switchB _   []         def = def
          switchB tag ((a,b):xs) def = ifB (tag ==* a) b (switchB tag xs def)

      fib <- recfunction $ \ fib (n :: JSNumber) ->
          ifB (n <* 2)
              (return (1 :: JSNumber))
              (liftM2 (+) (fib (n - 1)) (fib (n - 2)))

      addMethod "control" $ \ () ->
          wait "body" (slide <> click) $ \ res -> do
              model <- evaluate (obj ! "model") :: JS JSNumber

              switchB (res ! "id" :: JSString)
                  [ ("slider", update "model" (res ! "value") 0 25)
                  , ("up"    , update "model" (model + 1)     0 25)
                  , ("down"  , update "model" (model - 1)     0 25)
                  , ("reset" , update "model" 0               0 25)
                  ] $ return ()

              view ()

      addMethod "view" $ \ () -> do
          model <- evaluate (obj ! "model") :: JS JSNumber
          jQuery "#slider"  >>= slider (cast model)
          jQuery "#fib-out" >>= html ("fib " <> cast model <> "...")
          res <- apply fib model
          jQuery "#fib-out" >>= html ("fib " <> cast model <> " = " <> cast res)
          control ()

      control ()
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

recfunction :: (JSArgument a, Sunroof b) => ((a -> JS b) -> (a -> JS b)) -> JS (JSFunction a b)
recfunction fn = do
        obj <- new
        f <- function $ fn (\ n -> (obj <!> attribute "rec") >>= with n)
        obj # attribute "rec" := f
        return f


