{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Sunroof.Canvas where

import Language.Sunroof.Types

getContext :: JSString -> Action JSObject JSObject
getContext nm = method "getContext" [cast nm]

-- TODO: Add getters for properties.

-- -----------------------------------------------------------------------


-- | Draws a circular arc.
arc :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
    -> JSNumber            -- ^ The radius.
    -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
    -> JSBool              -- ^ If the arc shall be drawn counterclockwise.
    -> Action JSObject ()
arc (cx,cy) r (sa,ea) cc = 
  method "arc" [cast cx, cast cy, cast r, cast sa, cast ea, cast cc]

-- | Begins drawing a path or resets the current path
beginPath :: Action JSObject ()
beginPath = method "beginPath" []

-- | Draws a bezier curve beginning at the current position of the context.
bezierCurveTo :: (JSNumber,JSNumber) -- ^ The first control point.
              -> (JSNumber,JSNumber) -- ^ The second control point. 
              -> (JSNumber,JSNumber) -- ^ The endpoint of the curve.
              -> Action JSObject ()
bezierCurveTo (a,b) (c,d) (e,f) = 
  method "bezierCurveTo" [cast a, cast b, cast c, cast d, cast e, cast f]

-- | Clears the rectangle given by its location and size.
clearRect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectanlge to clear.
          -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle to clear.
          -> Action JSObject ()
clearRect (x,y) (w,h) = method "clearRect" [cast x, cast y, cast w, cast h]

-- | Closes the current path by drawing a straight line back to its beginning.
closePath :: Action JSObject ()
closePath = method "closePath" []

-- | Fills the current path with the current fill style.
fill :: Action JSObject ()
fill = method "fill" []

-- | Draws a filled rectangle given by its top left corner and size with the
--   current fill style.
fillRect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectangle.
         -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
         -> Action JSObject ()
fillRect (x,y) (w,h) = method "fillRect" [cast x, cast y, cast w, cast h]

-- | Sets the fill style of the context. A color value of the form "#XXXXXX" 
--   is expected.
fillStyle :: JSString -> Action JSObject ()
-- TODO: Add support for gradients and patterns.
fillStyle a = "fillStyle" := a

-- | Fills a text with the current fill style.
fillText :: JSString            -- ^ The text to fill.
         -> (JSNumber,JSNumber) -- ^ The x and y position of the 
                                --   bottom left corner of the text.
         -> Action JSObject () 
fillText s (x,y) = method "fillText" [cast s, cast x, cast y]

-- | Fills a text with the current fill style.
fillText' :: JSString             -- ^ The text to fill.
          -> (JSNumber, JSNumber) -- ^ The x and y position of the 
                                  --   bottom left corner of the text.
          -> JSNumber             -- ^ The maximum allowed width of the text.
          -> Action JSObject ()
fillText' s (x,y) maxW = method "fillText" [cast s, cast x, cast y, cast maxW]

-- | Sets the font used by the context.
font :: JSString -> Action JSObject ()
font f = "font" := f

-- | Sets the global alpha value.
globalAlpha :: JSNumber -> Action JSObject ()
globalAlpha a = "globalAlpha" := a

-- | Sets the line cap style to use.
lineCap :: JSString -> Action JSObject ()
lineCap lc = "lineCap" := lc

-- | Sets the line join style to use.
lineJoin :: JSString -> Action JSObject ()
lineJoin lj = "lineJoin" := lj

-- | Create a straight line path from the current point to the given point.
lineTo :: (JSNumber,JSNumber) -- ^ The x and y location the line is drawn to.
       -> Action JSObject ()
lineTo (x,y) = method "lineTo" [cast x, cast y]

-- | Sets the line width used when stroking.
lineWidth :: JSNumber           -- ^ The line new line width in pixels.
          -> Action JSObject ()
lineWidth lw = "lineWidth" := lw

-- | Sets the miter limit used when drawing a miter line join.
miterLimit :: JSNumber           -- ^ The new miter limit.
           -> Action JSObject ()
miterLimit ml = "miterLimit" := ml

-- | Move the path to the given location.
moveTo :: (JSNumber,JSNumber) -- ^ The new x and y location of the path.
       -> Action JSObject ()
moveTo (x,y) = method "moveTo" [cast x, cast y]

-- | Creates a rectangle in the current context.
rect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectangle.
     -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
     -> Action JSObject ()
rect (x,y) (w,h) = method "rect" [cast x, cast y, cast w, cast h]

-- | Restores the last saved paths and state of the context.
restore :: Action JSObject ()
restore = method "restore" []

-- | Rotates the current drawing. The rotation will only affect drawings
--   made after the rotation.
rotate :: JSNumber           -- ^ The rotation angle in radians.
       -> Action JSObject ()
rotate a = method "rotate" [cast a]

-- | Scales the current drawing.
scale :: (JSNumber,JSNumber) -- ^ The factors to scale the 
                             --   width and the height with.
      -> Action JSObject ()
scale (sw,sh) = method "scale" [cast sw, cast sh]

-- | Saves the state of the current context.
save :: Action JSObject ()
save = method "save" []

-- | Draws the current path using the current stroke style.
stroke :: Action JSObject ()
stroke = method "stroke" []

-- | Strokes a rectanlge using the current stroke style.
strokeRect :: (JSNumber,JSNumber) -- ^ The x and y coordinate of the top left corner.
           -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
           -> Action JSObject ()
strokeRect (x,y) (w,h) = method "strokeRect" [cast x, cast y, cast w, cast h]

-- | Strokes a text using the current stroke style.
strokeText :: JSString            -- ^ The text to stroke.
           -> (JSNumber,JSNumber) -- ^ The x and y coordinate to stroke the text at.
           -> Action JSObject ()
strokeText s (x,y) = method "strokeText" [cast s, cast x, cast y]

-- | Sets the stroke style of the context. A color value of the form "#XXXXXX" 
--   is expected.
strokeStyle :: JSString -> Action JSObject ()
-- TODO: Add support for patterns and gradients.
strokeStyle c = "strokeStyle" := c

-- | Sets the text alignment to be used when drawing text.
--   Possible values are: "center", "end", "left", "right", "start"
textAlign :: JSString -> Action JSObject ()
textAlign ta = "textAlign" := ta

-- | Sets the baseline to use when drawing text.
--   Possible values are: "alphabetic", "top", "hanging", "middle", "ideographic", "bottom"
textBaseline :: JSString -> Action JSObject ()
textBaseline tb = "textBaseline" := tb

-- | Alters the current transformation matrix. The current one is
--   multiplied with one of the form:
-- @
--   a b c
--   d e f
--   0 0 1
-- @
transform :: JSNumber -- ^ The 'a' value.
          -> JSNumber -- ^ The 'b' value.
          -> JSNumber -- ^ The 'c' value.
          -> JSNumber -- ^ The 'd' value.
          -> JSNumber -- ^ The 'e' value.
          -> JSNumber -- ^ The 'f' value.
          -> Action JSObject ()
transform a b c d e f = method "transform" [cast a,cast b,cast c, cast d, cast e,cast f]

-- | Translate the current drawing.
translate :: (JSNumber,JSNumber) -- ^ The x and y values to translate by.
          -> Action JSObject ()
translate (x,y) = method "translate" [cast x, cast y]
