{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty (scotty, middleware)
import Data.Default
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import Control.Applicative
import Data.Active
import Network.Wai.Middleware.Static
import Data.Boolean

import Web.KansasComet
import qualified Web.KansasComet as KC

import qualified Data.Boolean.Numbers as N


import Language.Sunroof
import Language.Sunroof.Types
import Language.Sunroof.JS.Canvas as C
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.JQuery
import Language.Sunroof.Active
import Language.Sunroof.Painting
import Language.Sunroof.Container
import Data.VectorSpace (lerp)

main :: IO ()
main = sunroofServer (defaultServerOpts { sunroofVerbose = 3, cometResourceBaseDir = ".." }) $ \ doc -> do
        registerEvents (cometDocument doc) "body" click
        async doc (example doc)

example :: SunroofEngine -> JS ()
example doc = do
  canvas <- document # getElementById "canvas"
  c <- canvas # getContext "2d"

  let clear :: Painting = painting $ \ c -> c # clearRect (0,0) (canvas ! width, canvas ! height)

  let drawing :: Active JSTime Painting = lineA (100,100) (300,300)

  width' <- canvas <!> width
  height' <- canvas <!> height

  let prog = pure clear <>
             ticTacToe (canvas ! width, canvas ! height)
{-
             pure (setLineWidthP 5) <>
             pure (setStrokeStyleP "#ff0000") <>
             (drawing ->> (scopeA (rotateA (2 * pi) <> lineA (300,100) (100,300))))
-}
  -- Stuff
  (s,e,f) <- reifyActiveJS $ fmap (draw c) $ scopeA $ prog

  n <- new
  n # "val" := (s :: JSNumber)
  loop <- function $ \ () -> do
                apply f (n ! "val")
                n # "val" := ((n ! "val") + 0.05 :: JSNumber)
                ifB ((n ! "val") >* (e + 0.025))
                    (window # clearInterval (n ! "callsign"))
                    (return ())
                return ()
  v <- window # setInterval loop 20
  n # "callsign" := v
  return ()

-- Active versions of painting commands
lineA :: (JSNumber,JSNumber) -> (JSNumber,JSNumber) -> Active JSTime Painting
lineA (x0,y0) (x1,y1) = clamp $ (\ (t::JSNumber) -> lineP (x0,y0) (lerp x0 x1 t,lerp y0 y1 t)) <$> ui

rotateA :: JSNumber -> Active JSTime Painting
rotateA speed = clamp $ (rotateP . (* speed)) <$> ui

translateA :: (JSNumber,JSNumber) -> Active JSTime Painting
translateA (a,b) = clamp $ (translateP . (\ t -> (a * t, b * t))) <$> ui

scopeA :: Active JSTime Painting -> Active JSTime Painting
scopeA = fmap scopeP

pauseA :: Active JSTime Painting                -- empty, just waits a second
pauseA = (\ (_ :: JSNumber) -> mempty) <$> ui

-- a number between -1 and 1 is returned
wobbleA :: JSNumber -> Active JSTime JSNumber
wobbleA speed = (\ (t :: JSNumber) -> sin ((t * speed) `N.mod` (pi * 2))) <$> ui

translateWA :: JSNumber -> Active JSTime Painting
translateWA n = (\ a b -> translateP (a,b)) <$> wobbleA (n*7) <*> wobbleA (n*13)



{-
arcA :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
     -> JSNumber            -- ^ The radius.
     -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
     -> JSBool              -- ^ if counter clock
     -> Painting
-}

default(JSNumber, JSString, String)

data Event = Click String Int Int
    deriving (Show)

click = event "click" Click
            <&> "id"      .= "$(widget).attr('id')"
            <&> "pageX"   .=  "event.pageX"
            <&> "pageY"   .=  "event.pageY"

ticTacToe :: (JSNumber,JSNumber) -> Active JSTime Painting
ticTacToe (width,height) = pure (translateP (width / 2, height / 2)) <>
        (stretch 3 backgroundGrid ->> play game (drawX,drawO))
  where
        scale = minB width height
        edge  = scale / 2.5     -- allow a small border
        step  = edge / 3
        pic   = step / 1.5

        game = [(x,y) | x <- map js [-1..(1::Int)], y <- map js [-1..(1::Int)]]
        play ((x,y):xys) (me,opp) = pauseA
                            ->> scopeA (pure (translateP (x*step*2,y*step*2)) <> me)
                            ->> play xys (opp,me)
        play _ _ = pauseA       --

        backgroundGrid =
                scopeA $ pure (setLineWidthP 10 <> setStrokeStyleP "#0000ff" <> painting (setLineCap "round")) <>
                         mconcat [ lineA (-edge,step*y) (edge,step*y) <>
                                   lineA (step*y,-edge) (step*y,edge)
                                 | y <- [1,-1] ]

                                 -- lineCap "butt"
        drawX :: Active JSTime Painting
        drawX = scopeA $ pure (setLineWidthP 5 <> setStrokeStyleP "#00ff00" <> painting (setLineCap "round")) <>
                                (lineA (-pic,-pic) (pic,pic) ->>
                                 lineA (-pic,pic) (pic,-pic))

        drawO :: Active JSTime Painting
        drawO = stretch 3
              $ scopeA
              $ pure (setLineWidthP 5 <> setStrokeStyleP "#ff0000" <> painting (setLineCap "round")) <>
                clamp ((\ (u :: JSNumber) -> arcP (0,0) pic (0,pi * 2 * u) false) <$> ui)

