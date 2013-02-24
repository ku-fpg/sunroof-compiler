{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Language.Sunroof.Painting where

import Language.Sunroof.Types
import Language.Sunroof.JS.Canvas

import Data.Monoid
import Data.Semigroup
import Data.Boolean

-- -----------------------------------------------------------------------

newtype Painting = Painting (JSObject -> JS ())

instance Monoid Painting where
        mempty = Painting $ const $ return ()
        mappend (Painting p1) (Painting p2) = Painting $ \ cxt -> do
                        p1 cxt
                        p2 cxt

instance Semigroup Painting where
        (<>) = mappend

type instance BooleanOf Painting = JSBool

instance IfB Painting where
        ifB b (Painting p1) (Painting p2) = Painting $ \ cxt -> ifB b (p1 cxt) (p2 cxt)


-- | draw a painting onto a (canvas) object.
draw :: JSObject -> Painting -> JS ()
draw obj (Painting fn) = fn obj

-- -----------------------------------------------------------------------
-- | Turn an action on a canvas into a painting.
-- Often, this action will be a compound action.
painting :: Action JSObject () -> Painting
painting act = Painting $ \ o -> o # act

-- -----------------------------------------------------------------------
-- Painting versions of common Canvas commands
-- -----------------------------------------------------------------------

scopeP :: Painting -> Painting
scopeP p = painting $ \ c -> do
        c # save
        draw c p
        c # restore

lineP :: (JSNumber,JSNumber) -> (JSNumber,JSNumber) -> Painting
lineP (x0,y0) (x1,y1) = painting $ \ c -> do
        c # beginPath
        c # moveTo (x0,y0)
        c # lineTo (x1,y1)
        c # stroke

arcP :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
     -> JSNumber            -- ^ The radius.
     -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
     -> JSBool              -- ^ if counter clock
     -> Painting
arcP (cx,cy) r (sa,ea) cc = painting $ \ c -> do
        c # beginPath
        c # arc' (cx,cy) r (sa,ea) cc
        c # stroke

rotateP :: JSNumber -> Painting
rotateP = painting . rotate

translateP :: (JSNumber,JSNumber) -> Painting
translateP = painting . translate

setLineWidthP :: JSNumber -> Painting
setLineWidthP = painting . setLineWidth

setStrokeStyleP :: JSString -> Painting
setStrokeStyleP = painting . setStrokeStyle

