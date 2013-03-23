
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provides bindings to the Javascript API of the 
--   HTML5 canvas element.
--   
--   See <http://www.w3schools.com/tags/ref_canvas.asp>.
module Language.Sunroof.JS.Canvas
  ( JSCanvas
  , getContext
  , arc, arc', arcTo
  , beginPath
  , bezierCurveTo
  , clearRect, clip
  , closePath
  , createImageData, createImageData'
  , drawImage, drawImage', drawImageClip
  , fill, fillRect
  , setFillStyle
  , fillText, fillText'
  , setFont
  , getImageData
  , setGlobalAlpha
  , isPointInPath
  , setLineCap, setLineJoin
  , lineTo
  , setLineWidth
  , miterLimit
  , setMiterLimit
  , measureText
  , moveTo
  , putImageData
  , rect
  , restore
  , rotate, scale
  , save
  , setTransform
  , setShadowColor, setShadowBlur
  , setShadowOffsetX, setShadowOffsetY
  , stroke, strokeRect, strokeText
  , setStrokeStyle
  , setTextAlign, setTextBaseline
  , transform, translate
  , quadraticCurveTo
  , width, height
  , data'
  , globalAlpha
  , shadowColor, shadowBlur
  , shadowOffsetX, shadowOffsetY
  , strokeStyle
  , textAlign, textBaseline
  , lineCap, lineJoin, lineWidth
  , font
  ) where

import Data.Boolean ( BooleanOf, IfB(..), EqB(..) )

import Language.Sunroof.Classes ( Sunroof(..) )
import Language.Sunroof.Types
import Language.Sunroof.Selector ( JSSelector, label )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.String ( JSString, string )
import Language.Sunroof.JS.Number ( JSNumber )

-- -------------------------------------------------------------
-- JSCanvas Type
-- -------------------------------------------------------------

-- | The type of the canvas drawing context.
newtype JSCanvas = JSCanvas JSObject

-- | Show the Javascript.
instance Show JSCanvas where
  show (JSCanvas o) = show o

-- | First-class values in Javascript.
instance Sunroof JSCanvas where
  box = JSCanvas . box
  unbox (JSCanvas o) = unbox o

-- | Associated boolean is 'JSBool'.
type instance BooleanOf JSCanvas = JSBool

-- | Can be returned in branches.
instance IfB JSCanvas where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSCanvas where
  (JSCanvas a) ==* (JSCanvas b) = a ==* b

-- -------------------------------------------------------------
-- JSCanvas Combinators
-- -------------------------------------------------------------

-- | Returns the canvas drawing context for the canvas element it
--   is called on.
getContext :: JSString -> JSObject -> JS t JSCanvas
getContext nm = invoke "getContext" nm

-- | Draws a circular arc.
arc :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
    -> JSNumber            -- ^ The radius.
    -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
    -> JSCanvas -> JS t ()
arc (cx,cy) r (sa,ea) =
  invoke "arc" (cx, cy, r, sa, ea)

-- | Draws a circular arc.
arc' :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
    -> JSNumber            -- ^ The radius.
    -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
    -> JSBool              -- ^ If the arc shall be drawn counterclockwise.
    -> JSCanvas -> JS t ()
arc' (cx,cy) r (sa,ea) cc =
  invoke "arc" (cx, cy, r, sa, ea, cc)

-- | Creates an arc between two tangents on the canvas.
arcTo :: (JSNumber, JSNumber) -- ^ The x and y coordinate of the beginning of the arc.
      -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the end of the arc.
      -> JSNumber             -- ^ The radius of the arc.
      -> JSCanvas -> JS t ()
arcTo (x1, y1) (x2, y2) r =
  invoke "arcTo" (x1, y1, x2, y2, r)

-- | Begins drawing a path or resets the current path
beginPath :: JSCanvas -> JS t ()
beginPath = invoke "beginPath" ()

-- | Draws a bezier curve beginning at the current position of the context.
bezierCurveTo :: (JSNumber,JSNumber) -- ^ The first control point.
              -> (JSNumber,JSNumber) -- ^ The second control point.
              -> (JSNumber,JSNumber) -- ^ The endpoint of the curve.
              -> JSCanvas -> JS t ()
bezierCurveTo (a,b) (c,d) (e,f) =
  invoke "bezierCurveTo" (a, b, c, d, e, f)

-- | Clears the rectangle given by its location and size.
clearRect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectanlge to clear.
          -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle to clear.
          -> JSCanvas -> JS t ()
clearRect (x,y) (w,h) = invoke "clearRect" (x, y, w, h)

-- | Clips a region of any shape and size from the context.
clip :: JSCanvas -> JS t ()
clip = invoke "clip" ()

-- | Closes the current path by drawing a straight line back to its beginning.
closePath :: JSCanvas -> JS t ()
closePath = invoke "closePath" ()

-- | Create a new image data object with the given size.
createImageData :: (JSNumber, JSNumber)     -- ^ The width and hight of the new object.
                -> JSCanvas -> JS t JSObject -- ^ Returns the new image data object.
createImageData (w,h) = invoke "createImageData" (w, h)

-- | Creates a new image data object with the same dimension as the given
--   image data object. This does not copy the contents of the other object.
createImageData' :: JSObject                 -- ^ The other image data object.
                 -> JSCanvas -> JS t JSObject -- ^ Returns the new image data object.
createImageData' imgData = invoke "createImageData" (imgData)

-- | Draws an image, video or canvas to the canvas.
drawImage :: JSObject             -- ^ The graphical object to draw.
          -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
          -> JSCanvas -> JS t ()
drawImage img (x,y) = invoke "drawImage" (img, x, y)

-- | Draws an image, video or canvas to the canvas.
drawImage' :: JSObject             -- ^ The graphical object to draw.
           -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
           -> (JSNumber, JSNumber) -- ^ The width and height to scale the image to.
           -> JSCanvas -> JS t ()
drawImage' img (x,y) (w,h) =
  invoke "drawImage" (img, x, y, w, h)

-- | Draws an image, video or canvas to the canvas. Clips the drawn object.
drawImageClip :: JSObject          -- ^ The graphical object to draw.
              -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top
                                      --   left corner of the clippng area.
              -> (JSNumber, JSNumber) -- ^ The width and height of the clipping area
              -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
              -> (JSNumber, JSNumber) -- ^ The width and height to scale the image to.
              -> JSCanvas -> JS t ()
drawImageClip img (cx, cy) (cw, ch) (x,y) (w,h) =
  invoke "drawImage" ( img
                     , cx, cy
                     , cw, ch
                     , x, y
                     , w, h)

-- | Fills the current path with the current fill style.
fill :: JSCanvas -> JS t ()
fill = invoke "fill" ()

-- | Draws a filled rectangle given by its top left corner and size with the
--   current fill style.
fillRect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectangle.
         -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
         -> JSCanvas -> JS t ()
fillRect (x,y) (w,h) = invoke "fillRect" (x, y, w, h)

-- | Sets the fill style of the context. A color value of the form "#XXXXXX"
--   is expected.
setFillStyle :: JSString -> JSCanvas -> JS t ()
-- TODO: Add support for gradients and patterns.
setFillStyle fs = "fillStyle" := fs

-- | Fills a text with the current fill style.
fillText :: JSString            -- ^ The text to fill.
         -> (JSNumber,JSNumber) -- ^ The x and y position of the
                                --   bottom left corner of the text.
         -> JSCanvas -> JS t ()
fillText s (x,y) = invoke "fillText" (s, x, y)

-- | Fills a text with the current fill style.
fillText' :: JSString             -- ^ The text to fill.
          -> (JSNumber, JSNumber) -- ^ The x and y position of the
                                  --   bottom left corner of the text.
          -> JSNumber             -- ^ The maximum allowed width of the text.
          -> JSCanvas -> JS t ()
fillText' s (x,y) maxW = invoke "fillText" (s, x, y, maxW)

-- | Sets the font used by the context.
setFont :: JSString -> JSCanvas -> JS t ()
setFont f = "font" := f

-- | Get the image data of the specified rectanlge of the canvas.
getImageData :: (JSNumber, JSNumber)     -- ^ The x and y coordinate of the top left
                                         --   corner of the rectangle to extract.
             -> (JSNumber, JSNumber)     -- ^ The width and height of the rectangle.
             -> JSCanvas -> JS t JSObject -- ^ Returns the image data object
                                         --   with the extracted information.
getImageData (x,y) (w,h) =
  invoke "getImageData" (x, y, w , h)

-- | Sets the global alpha value.
setGlobalAlpha :: JSNumber -> JSCanvas -> JS t ()
setGlobalAlpha a = "globalAlpha" := a

-- | Returns true if the given point is in the path and false otherwise.
isPointInPath :: (JSNumber, JSNumber)   -- ^ The x and y coordinate of the point to check
              -> JSCanvas -> JS t JSBool
isPointInPath (x,y) = invoke "isPointInPath" (x, y)

-- | Sets the line cap style to use.
--   Possible values are: "butt", "round", "square";
setLineCap :: JSString -> JSCanvas -> JS t ()
setLineCap lc = "lineCap" := lc

-- | Sets the line join style to use.
--   Possible values are: "bevel", "round", "meter";
setLineJoin :: JSString -> JSCanvas -> JS t ()
setLineJoin lj = "lineJoin" := lj

-- | Create a straight line path from the current point to the given point.
lineTo :: (JSNumber,JSNumber) -- ^ The x and y location the line is drawn to.
       -> JSCanvas -> JS t ()
lineTo (x,y) = invoke "lineTo" (x, y)

-- | Sets the line width used when stroking.
setLineWidth :: JSNumber           -- ^ The line new line width in pixels.
          -> JSCanvas -> JS t ()
setLineWidth lw = "lineWidth" := lw

-- | Returns the miter limit used when drawing a miter line join.
miterLimit :: JSSelector JSNumber
miterLimit = label $ string "miterLimit"

-- | Sets the miter limit used when drawing a miter line join.
setMiterLimit :: JSNumber           -- ^ The new miter limit.
              -> JSCanvas -> JS t ()
setMiterLimit ml = "miterLimit" := ml

-- | Returns an object that contains the width of the specified text is pixels.
--   See 'width' selector.
measureText :: JSString                 -- ^ The text to be measured.
            -> JSCanvas -> JS t JSObject
measureText s = invoke "measureText" (s)

-- | Move the path to the given location.
moveTo :: (JSNumber,JSNumber) -- ^ The new x and y location of the path.
       -> JSCanvas -> JS t ()
moveTo (x,y) = invoke "moveTo" (x, y)

-- | Uses the given image data to replace the rectangle of the
--   canvas at the given position.
putImageData :: JSObject             -- ^ The new image data.
             -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
             -> JSCanvas -> JS t ()
putImageData imgData (x,y) = invoke "putImageData" (imgData, x, y)

-- | Creates a rectangle in the current context.
rect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectangle.
     -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
     -> JSCanvas -> JS t ()
rect (x,y) (w,h) = invoke "rect" (x, y, w, h)

-- | Restores the last saved paths and state of the context.
restore :: JSCanvas -> JS t ()
restore = invoke "restore" ()

-- | Rotates the current drawing. The rotation will only affect drawings
--   made after the rotation.
rotate :: JSNumber           -- ^ The rotation angle in radians.
       -> JSCanvas -> JS t ()
rotate a = invoke "rotate" (a)

-- | Scales the current drawing.
scale :: (JSNumber,JSNumber) -- ^ The factors to scale the
                             --   width and the height with.
      -> JSCanvas -> JS t ()
scale (sw,sh) = invoke "scale" (sw, sh)

-- | Saves the state of the current context.
save :: JSCanvas -> JS t ()
save = invoke "save" ()

-- | Resets the transformation matrix to identity and then applies
--   'transform' with the given paramters to it.
setTransform :: JSNumber -- ^ Scales the drawings horizontally.
             -> JSNumber -- ^ Skew the drawings horizontally.
             -> JSNumber -- ^ Skew the drawings vertically.
             -> JSNumber -- ^ Scales the drawings vertically.
             -> JSNumber -- ^ Moves the drawings horizontally.
             -> JSNumber -- ^ Moves the drawings vertically.
             -> JSCanvas -> JS t ()
setTransform a b c d e f =
  invoke "setTransform" (a, b, c, d, e, f)

-- | Sets the shadow color property.
--   The given string has to be a valid CSS color value or a
--   color of the form '#XXXXXX'
setShadowColor :: JSString           -- ^ The color to use as shadow color.
            -> JSCanvas -> JS t ()
setShadowColor c = "shadowColor" := c

-- | Sets the blur level for shadows.
setShadowBlur :: JSNumber           -- ^ The blur level for the shadow in pixels.
           -> JSCanvas -> JS t ()
setShadowBlur b = "shadowBlur" := b

-- | Sets the x offset of a shadow from a shape.
setShadowOffsetX :: JSNumber           -- ^ The x offset of the shadow in pixels.
              -> JSCanvas -> JS t ()
setShadowOffsetX x = "shadowOffsetX" := x

-- | Sets the y offset of a shadow from a shape.
setShadowOffsetY :: JSNumber           -- ^ The y offset of the shadow in pixels.
              -> JSCanvas -> JS t ()
setShadowOffsetY y = "shadowOffsetY" := y

-- | Draws the current path using the current stroke style.
stroke :: JSCanvas -> JS t ()
stroke = invoke "stroke" ()

-- | Strokes a rectanlge using the current stroke style.
strokeRect :: (JSNumber,JSNumber) -- ^ The x and y coordinate of the top left corner.
           -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
           -> JSCanvas -> JS t ()
strokeRect (x,y) (w,h) = invoke "strokeRect" (x, y, w, h)

-- | Strokes a text using the current stroke style.
strokeText :: JSString            -- ^ The text to stroke.
           -> (JSNumber,JSNumber) -- ^ The x and y coordinate to stroke the text at.
           -> JSCanvas -> JS t ()
strokeText s (x,y) = invoke "strokeText" (s, x, y)

-- | Sets the stroke style of the context. A color value of the form "#XXXXXX"
--   is expected.
setStrokeStyle :: JSString -> JSCanvas -> JS t ()
-- TODO: Add support for patterns and gradients.
setStrokeStyle c = "strokeStyle" := c

-- | Sets the text alignment to be used when drawing text.
--   Possible values are: "center", "end", "left", "right", "start"
setTextAlign :: JSString -> JSCanvas -> JS t ()
setTextAlign ta = "textAlign" := ta

-- | Sets the baseline to use when drawing text.
--   Possible values are: "alphabetic", "top", "hanging", "middle", "ideographic", "bottom"
setTextBaseline :: JSString -> JSCanvas -> JS t ()
setTextBaseline tb = "textBaseline" := tb

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
          -> JSCanvas -> JS t ()
transform a b c d e f =
  invoke "transform" (a, b, c, d, e, f)

-- | Translate the current drawing.
translate :: (JSNumber,JSNumber) -- ^ The x and y values to translate by.
          -> JSCanvas -> JS t ()
translate (x,y) = invoke "translate" (x, y)

-- | Create a quadratic curve to extend the current path.
quadraticCurveTo :: (JSNumber, JSNumber) -- ^ The control point of the curve.
                 -> (JSNumber, JSNumber) -- ^ The endpoint of the curve.
                 -> JSCanvas -> JS t ()
quadraticCurveTo (cx, cy) (ex, ey) =
  invoke "quadraticCurveTo" (cx, cy, ex, ey)

-- | Selects the width attribute.
width :: JSSelector JSNumber
width = attr "width"

-- | Selects the height attribute.
height :: JSSelector JSNumber
height = attr "height"

-- | Selects the data attribute.
data' :: JSSelector JSObject
data' = attr "data"

-- | Selects the global alpha attribute.
globalAlpha :: JSSelector JSNumber
globalAlpha = attr "globalAlpha"

-- | Selects the shadow color attribute.
shadowColor :: JSSelector JSString
shadowColor = attr "shadowColor"

-- | Selects the blur level for shadows.
shadowBlur :: JSSelector JSNumber
shadowBlur = attr "shadowBlur"

-- | Selects the x offset of a shadow from a shape.
shadowOffsetX :: JSSelector JSNumber
shadowOffsetX = attr "shadowOffsetX"

-- | Selects the y offset of a shadow from a shape.
shadowOffsetY :: JSSelector JSNumber
shadowOffsetY = attr "shadowOffsetY"

-- | Selects the stroke style of the context.
strokeStyle :: JSSelector JSString
-- TODO: Add support for patterns and gradients.
strokeStyle = attr "strokeStyle"

-- | Selects the text alignment to be used when drawing text.
--   Possible values are: "center", "end", "left", "right", "start"
textAlign :: JSSelector JSString
textAlign = attr "textAlign"

-- | Selects the baseline to use when drawing text.
--   Possible values are: "alphabetic", "top", "hanging", "middle", "ideographic", "bottom"
textBaseline :: JSSelector JSString
textBaseline = attr "textBaseline"

-- | Sets the line cap style to use.
--   Possible values are: "butt", "round", "square";
lineCap :: JSSelector JSString
lineCap = attr "lineCap"

-- | Selects the line join style to use.
--   Possible values are: "bevel", "round", "meter";
lineJoin :: JSSelector JSString
lineJoin = attr "lineJoin"

-- | Selects the line width used when stroking.
lineWidth :: JSSelector JSNumber
lineWidth = attr "lineWidth"

-- | Selects the font used by the context.
font :: JSSelector JSString
font = attr "font"

