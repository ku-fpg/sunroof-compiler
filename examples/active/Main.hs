{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Web.Scotty (scotty, middleware)
import Data.Default
import Control.Monad hiding (forever)
import Data.Semigroup
import Control.Monad.IO.Class
import Control.Applicative
import Data.Active
import Network.Wai.Middleware.Static
import Data.Boolean
import qualified Data.Boolean.Numbers as Deep

import Web.KansasComet
import qualified Web.KansasComet as KC

import qualified Data.Boolean.Numbers as N


import Language.Sunroof
import Language.Sunroof.Types
import Language.Sunroof.JS.Canvas as C
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.JQuery
import Language.Sunroof.Active
--import Language.Sunroof.Painting
import Language.Sunroof.Container
import Data.VectorSpace (lerp)

main :: IO ()
main = sunroofServer (defaultServerOpts { sunroofVerbose = 3, cometResourceBaseDir = ".." }) $ \ doc -> do
        registerEvents (cometDocument doc) "body" (slide <> click)
        async doc (example doc)

example :: SunroofEngine -> JS A ()
example doc = do
  canvas <- document # getElementById "canvas"
  c <- canvas # getContext "2d"

  let clear :: Painting = clearRect (0,0) (canvas ! width, canvas ! height)

  width' <- evaluate $ canvas ! width
  height' <- evaluate $ canvas ! height

  let prog :: Active JSTime Painting
      prog = pure clear <>
             ticTacToe (canvas ! width, canvas ! height)
--               counter (canvas ! width, canvas ! height)

  let full :: Active JSTime Painting
      full = prog <> (timeline (100,200) (300,220) `during` prog)

  (s,e,f) <- reifyActiveJS $ fmap (c #) $ scopeA $ full

  let mul :: JSNumber
      mul = 400 / (e - s)

  jQuery "#start" >>= text (cast s)
  jQuery "#end"   >>= text (cast e)

--  alert("Count" <> cast s <> " " <> cast e)

  date            <- evaluate $ object "new Date()"
  tm0 :: JSNumber <- date # method "getTime" ()

  jQuery "#slider" >>= method "slider" ("option" :: JSString, "min" :: JSString, s * mul :: JSNumber) :: JS A ()
  jQuery "#slider" >>= method "slider" ("option" :: JSString, "max" :: JSString, e * mul :: JSNumber) :: JS A ()

{-
  n <- new
  n # "val" := (s :: JSNumber)
  let speed :: Rational
      speed = 20        -- FPS target
  loop <- function $ \ () -> do
                date            <- evaluate $ object "new Date()"
                tm1 :: JSNumber <- date # method "getTime" ()
                let tm = (tm1 - tm0) / 1000
                ifB (tm >* e)
                    (window # clearInterval (n ! "callsign"))
                    (return ())
                printFixed "#time" 2 tm
                apply f tm
                jQuery "#slider"  >>= method "slider" ("option" :: JSString, "value" :: JSString, Deep.floor (tm * mul) :: JSNumber) :: JS ()
                return ()
-}


{-
  let loop2 m = do
          alert("X")
          wait "body" (slide <> click) $ \ res -> do
                        alert("X")
                        alert("X" <> cast res)
                        m
--                        forkJS loop2

  forkJS $ do
          loop2 (
-}
  let paint tm = do
          printFixed "#time" 2 tm
          apply f tm

  forkJS $ loopJS $ do
          res <- wait "body" (slide <> click)
          liftJS $ do
             val :: JSNumber <- evaluate (res ! "value")
             let nm = val / mul
             paint nm
{-
          switchB ((res ! "id" :: JSString))
            [("slider", liftJSB $ do
                  val :: JSNumber <- evaluate (res ! "value")
                  let nm = val / mul
                  paint nm)
            ,("player", do
                     stopJSB
             )]
-}
{-

-}

switchB _   []         = return ()
switchB tag ((a,b):xs) = ifB (tag ==* a) b (switchB tag xs)

--  v <- window # setInterval loop (fromRational (1000 / speed))
--  n # "callsign" := v


printFixed :: JSString -> JSNumber -> JSNumber -> JS A ()
printFixed tag prec val = do
        val' <- cast val # method "toFixed" prec
        jQuery tag >>= text val'
        return ()

-- Active versions of painting commands
lineA :: (JSNumber,JSNumber) -> (JSNumber,JSNumber) -> Active JSTime Painting
lineA (x0,y0) (x1,y1) = clamp $ (\ (t::JSNumber) -> lineP (x0,y0) (lerp x0 x1 t,lerp y0 y1 t)) <$> ui

rotateA :: JSNumber -> Active JSTime Painting
rotateA speed = clamp $ (rotate . (* speed)) <$> ui

translateA :: (JSNumber,JSNumber) -> Active JSTime Painting
translateA (a,b) = clamp $ (translate . (\ t -> (a * t, b * t))) <$> ui

scopeA :: Active JSTime Painting -> Active JSTime Painting
scopeA = fmap scopeP

pauseA :: Active JSTime Painting                -- empty, just waits a second
pauseA = (\ (_ :: JSNumber) -> mempty) <$> ui

-- a number between -1 and 1 is returned
wobbleA :: JSNumber -> Active JSTime JSNumber
wobbleA speed = (\ (t :: JSNumber) -> sin ((t * speed) `N.mod` (pi * 2))) <$> ui

translateWA :: JSNumber -> Active JSTime Painting
translateWA n = (\ a b -> translate (a,b)) <$> wobbleA (n*7) <*> wobbleA (n*13)

type Painting = JSObject -> JS A ()     -- painting does not block

{-
arcA :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
     -> JSNumber            -- ^ The radius.
     -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
     -> JSBool              -- ^ if counter clock
     -> Painting
-}

default(JSNumber, JSString, String)

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

-- counter

counter :: (JSNumber,JSNumber) -> Active JSTime Painting
counter (width,height)
        = scopeA $ pure (translate (width / 3, height / 3)) <>
                        (stretch 100 countPlease)
  where
          countPlease :: Active JSTime Painting
          countPlease = clamp $ scopeA $
                pure ((setFont "40pt Calibri")) <>
                     (\ (n :: JSNumber) -> \ c -> do
--                                        alert(cast n)
                                        c # fillText (cast (Deep.floor (n*100))) (n*100,0)
                                        date <- evaluate $ object "new Date()"
                                        s <- date # method "getSeconds" ()
                                        c # fillText (cast (s :: JSNumber)) (n*100,100)
--                                        alert("X" <> cast n)
                     ) <$> ui
--                     painting $ fillText ("X") (n*10,0)) <$> ui
--                ("X: " {- <> cast (Deep.floor (n :: JSNumber)) -})

ticTacToe :: (JSNumber,JSNumber) -> Active JSTime Painting
ticTacToe (width,height) =
        scopeA $ pure (translate (width / 2, height / 2)) <>
                      (stretch 3 backgroundGrid ->> play game (drawX,drawO))
  where
        scale = minB width height
        edge  = scale / 2.5     -- allow a small border
        step  = edge / 3
        pic   = step / 1.5

        game :: [(JSNumber,JSNumber)]
        game = [ (-1,-1),(0,0)
               , (1,1), (-1,1)
               , (1,-1), (1,0)
               , (0,-1)
               ]

        play ((x,y):xys) (me,opp) = pauseA
                            ->> scopeA (pure (translate (x*step*2,y*step*2)) <> me)
                            ->> play xys (opp,me)
        play _ _ = pauseA ->> scopeA (pure ((translate (0,-step*2))) <> winningLine)

        backgroundGrid :: Active JSTime Painting
        backgroundGrid =
                scopeA $ pure (setLineWidth 10 <> setStrokeStyle "#0000ff" <> setLineCap "round") <>
                         mconcat [ lineA (-edge,step*y) (edge,step*y) <>
                                   lineA (step*y,-edge) (step*y,edge)
                                 | y <- [1,-1] ]

                                 -- lineCap "butt"
        drawX :: Active JSTime Painting
        drawX = clamp $
                scopeA $ pure (setLineWidth 5 <> setStrokeStyle "#00ff00" <> setLineCap "round") <>
                                (lineA (-pic,-pic) (pic,pic) ->>
                                 lineA (-pic,pic) (pic,-pic))

        drawO :: Active JSTime Painting
        drawO = stretch 3
              $ scopeA
              $ pure (setLineWidth 5 <> setStrokeStyle "#ff0000" <> setLineCap "round") <>
                         pure (( setShadowColor "black"  <>
                                              setShadowBlur 10 <>
                                              setShadowOffsetX 2 <>
                                              setShadowOffsetY 2)) <>
                clamp ((\ (u :: JSNumber) -> arcP (0,0) pic (0,pi * 2 * u) false) <$> ui)

        winningLine :: Active JSTime Painting
        winningLine = pure (setLineWidth 8 <> setStrokeStyle "#000000" <>  (setLineCap "butt")) <>
                        translateWA 1 <>
                        lineA (-step * 3.2,0) (step * 3.2,0)


timeline :: (JSNumber,JSNumber) -> (JSNumber,JSNumber) -> Active JSTime Painting
timeline (x0,y0) (x1,y1) =
        scopeA $ pure (setLineWidth 2 <> setStrokeStyle "#000000" <>  (setLineCap "butt")) <>
                 (clamp $ (\ (t::JSNumber) -> lineP (x0,y0) (x1,y1)) <$> ui) <>
                 pure (setLineWidth 8 <> setStrokeStyle "#ff0000" <>  (setLineCap "butt")) <>
                 (clamp $ (\ (t::JSNumber) -> lineP (x0,y0) (lerp x0 x1 t,lerp y0 y1 t)) <$> ui)


arcP :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
     -> JSNumber            -- ^ The radius.
     -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
     -> JSBool              -- ^ if counter clock
     -> Painting
arcP (cx,cy) r (sa,ea) cc = \ c -> do
        c # beginPath
        c # arc' (cx,cy) r (sa,ea) cc
        c # stroke

scopeP :: Painting -> Painting
scopeP p = \ c -> do
        c # save
        c # p
        c # restore

lineP :: (JSNumber,JSNumber) -> (JSNumber,JSNumber) -> Painting
lineP (x0,y0) (x1,y1) = \ c -> do
        c # beginPath
        c # moveTo (x0,y0)
        c # lineTo (x1,y1)
        c # stroke

