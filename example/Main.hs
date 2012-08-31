{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- Example of using Kansas Comet

module Main where

import qualified Web.Scotty as Scotty
import Web.Scotty (scottyOpts, get, file, middleware)
import Data.Default
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import Network.Wai.Middleware.Static
import Data.Boolean

-- import Network.Wai      -- TMP for debug

import Web.KansasComet hiding ((:=))
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.Compiler
import Language.Sunroof.Types

import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

main :: IO ()
main = do
    -- build the scotty dispatch app
    scottyOpts (def { Scotty.verbose = 0 })  $ do
        -- provide some static pages, include jquery
        -- This is scotty code

        let pol = only [("","index.html")]
                  <|> ((hasPrefix "css/" <|> hasPrefix "js/") >-> addBase ".")

        middleware $ staticPolicy pol

        kcomet <- liftIO kCometPlugin
        get "/js/kansas-comet.js" $ file $ kcomet
        -- connect /example to the following web_app
        connect opts web_app

opts :: KC.Options
opts = def { prefix = "/example", verbose = 0 }

default(JSNumber, JSString, String)

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
        print "web_app"

        registerEvents doc "body" (slide <> click)

        sync doc $ do
                obj <- new
                obj <$> "model" := (0 :: JSNumber)

                -- This is using the imperative update to enable the
                let control :: () -> JS ()
                    control () = obj ! "control" <$> with []

                    view :: () -> JS ()
                    view () = obj ! "view" <$> with []

                    set :: (Sunroof a, Sunroof b) => String -> (a -> JS b) -> JS ()
                    set nm f = do n <- function f
                                  obj <$> nm := n

                    eventname :: JSSelector JSString
                    eventname = label "eventname"

                    jQuery :: JSString -> JS JSObject
                    jQuery nm = call "$" <$> with [cast nm]

                    slider :: JSValue -> Action JSObject JSObject
                    slider nm = method "slider"  ["value", cast nm]

                    html :: JSString -> Action JSObject JSObject
                    html nm = method "html"  [cast nm]

                    fib :: JSNumber -> JS JSNumber
                    fib n = obj ! "fib" <$> with [cast n]

                set "fib" $ \ (n :: JSNumber) ->
                        ifB (n <* 2)
                                (return 1)
                                (liftM2 (+) (fib (n - 1)) (fib (n - 2)))

                set "control" $ \ () -> wait "body" (slide <> click) $ \ event -> do
                        model <- eval (obj ! "model") :: JS JSNumber
                        model' <- ifB ((event ! eventname) ==* "slide")
                                        (return (event ! "value" :: JSNumber))
                                $ ifB ((event ! "id" :: JSString) ==* "up" &&* model <* 25)
                                        (return (model + 1))
                                $ ifB ((event ! "id" :: JSString) ==* "down" &&* model >* 0)
                                        (return (model - 1))
                                $ ifB ((event ! "id" :: JSString) ==* "reset")
                                        (return 0)
                                $ (return model)
                        obj <$> "model" := model'
                        view ()

                set "view" $ \ () -> do
                        model <- eval (obj ! "model") :: JS JSNumber
                        jQuery "#slider"  <*> slider (cast model)
                        jQuery "#fib-out" <*> html ("fib " <> cast model <> "...")
                        res <- fib model
                        jQuery "#fib-out" <*> html ("fib " <> cast model <> " = " <> cast res)
                        control ()

                -- call control
                control ()
        return ()

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Event = Slide String Int
           | Click String Int Int
    deriving (Show)

(~=) = (KC.:=)

slide = event "slide" Slide
            <&> "id"      ~= "$(widget).attr('id')"
            <&> "value"   ~= "aux.value"

click = event "click" Click
            <&> "id"      ~= "$(widget).attr('id')"
            <&> "pageX"   ~=  "event.pageX"
            <&> "pageY"   ~=  "event.pageY"

