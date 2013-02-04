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
    registerEvents doc "body" (slide <> click)
    {- Playing with canvas...
    let getElementById :: JSString -> Action JSObject JSObject
        getElementById a = method "getElementById" [cast a]

    sync doc $ do
        canvas <- object "document" <$> getElementById "canvas"
        context <- canvas <$> getContext "2d"
        context <$> setFillStyle "rgba(0, 0, 255, .5)"
        context <$> fillRect (25, 25) (125, 125)

        alert (cast context)

        return ()
        -}
    async doc $ do
        obj <- new
        obj <$> "model" := (0 :: JSNumber)

        -- This is using the imperative update to enable the
        let control :: () -> JS ()
            control () = obj ! "control" <$> with []

            view :: () -> JS ()
            view () = obj ! "view" <$> with []

            addMethod :: (Sunroof a, Sunroof b) => String -> (a -> JS b) -> JS ()
            addMethod nm f = do n <- function f
                                obj <$> nm := n

            jQuery :: JSString -> JS JSObject
            jQuery nm = call "$" <$> with [cast nm]

            slider :: JSValue -> Action JSObject JSObject
            slider nm = method "slider"  ["value", cast nm]

            html :: JSString -> Action JSObject JSObject
            html nm = method "html"  [cast nm]

            fib :: JSNumber -> JS JSNumber
            fib n = obj ! "fib" <$> with [cast n]

            update :: String -> JSNumber -> JSNumber -> JSNumber -> JS ()
            update nm val mn mx =
                ifB ((val <=* mx) &&* (val >=* mn))
                    (obj <$> nm := val)
                    (return ())

            switchB _   []         def = def
            switchB tag ((a,b):xs) def = ifB (tag ==* a) b (switchB tag xs def)

        addMethod "fib" $ \ (n :: JSNumber) ->
            ifB (n <* 2)
                (return 1)
                (liftM2 (+) (fib (n - 1)) (fib (n - 2)))

        addMethod "control" $ \ () ->
            wait "body" (slide <> click) $ \ res -> do
                model <- eval (obj ! "model") :: JS JSNumber

                switchB (res ! "id" :: JSString)
                    [ ("slider", update "model" (res ! "value") 0 25)
                    , ("up"    , update "model" (model + 1)     0 25)
                    , ("down"  , update "model" (model - 1)     0 25)
                    , ("reset" , update "model" 0               0 25)
                    ] $ return ()

                view ()

        addMethod "view" $ \ () -> do
            model <- eval (obj ! "model") :: JS JSNumber
            jQuery "#slider"  <*> slider (cast model)
            jQuery "#fib-out" <*> html ("fib " <> cast model <> "...")
            res <- fib model
            jQuery "#fib-out" <*> html ("fib " <> cast model <> " = " <> cast res)
            control ()

        control ()
    return ()

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

