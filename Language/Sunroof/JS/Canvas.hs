{-# LANGUAGE OverloadedStrings #-}

module Language.Sunroof.JS.Canvas where

import Language.Sunroof.Types

getContext :: JSString -> Action JSObject JSObject
getContext nm = method "getContext" (nm)

-- -----------------------------------------------------------------------

-- | Draws a circular arc.
arc :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
    -> JSNumber            -- ^ The radius.
    -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
    -> Action JSObject ()
arc (cx,cy) r (sa,ea) =
  method "arc" (cx, cy, r, sa, ea)

-- | Draws a circular arc.
arc' :: (JSNumber,JSNumber) -- ^ The x and y component of the center point.
    -> JSNumber            -- ^ The radius.
    -> (JSNumber,JSNumber) -- ^ The angle to start and the angle to stop drawing.
    -> JSBool              -- ^ If the arc shall be drawn counterclockwise.
    -> Action JSObject ()
arc' (cx,cy) r (sa,ea) cc =
  method "arc" (cx, cy, r, sa, ea, cc)

-- | Creates an arc between two tangents on the canvas.
arcTo :: (JSNumber, JSNumber) -- ^ The x and y coordinate of the beginning of the arc.
      -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the end of the arc.
      -> JSNumber             -- ^ The radius of the arc.
      -> Action JSObject ()
arcTo (x1, y1) (x2, y2) r =
  method "arcTo" (x1, y1, x2, y2, r)

-- | Begins drawing a path or resets the current path
beginPath :: Action JSObject ()
beginPath = method "beginPath" ()

-- | Draws a bezier curve beginning at the current position of the context.
bezierCurveTo :: (JSNumber,JSNumber) -- ^ The first control point.
              -> (JSNumber,JSNumber) -- ^ The second control point.
              -> (JSNumber,JSNumber) -- ^ The endpoint of the curve.
              -> Action JSObject ()
bezierCurveTo (a,b) (c,d) (e,f) =
  method "bezierCurveTo" (a, b, c, d, e, f)

-- | Clears the rectangle given by its location and size.
clearRect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectanlge to clear.
          -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle to clear.
          -> Action JSObject ()
clearRect (x,y) (w,h) = method "clearRect" (x, y, w, h)

-- | Clips a region of any shape and size from the context.
clip :: Action JSObject ()
clip = method "clip" ()

-- | Closes the current path by drawing a straight line back to its beginning.
closePath :: Action JSObject ()
closePath = method "closePath" ()

-- | Create a new image data object with the given size.
createImageData :: (JSNumber, JSNumber)     -- ^ The width and hight of the new object.
                -> Action JSObject JSObject -- ^ Returns the new image data object.
createImageData (w,h) = method "createImageData" (w, h)

-- | Creates a new image data object with the same dimension as the given
--   image data object. This does not copy the contents of the other object.
createImageData' :: JSObject                 -- ^ The other image data object.
                 -> Action JSObject JSObject -- ^ Returns the new image data object.
createImageData' imgData = method "createImageData" (imgData)

-- | Draws an image, video or canvas to the canvas.
drawImage :: JSObject             -- ^ The graphical object to draw.
          -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
          -> Action JSObject ()
drawImage img (x,y) = method "drawImage" (img, x, y)

drawImage' :: JSObject             -- ^ The graphical object to draw.
           -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
           -> (JSNumber, JSNumber) -- ^ The width and height to scale the image to.
           -> Action JSObject ()
drawImage' img (x,y) (w,h) =
  method "drawImage" (img, x, y, w, h)

drawImageClip :: JSObject          -- ^ The graphical object to draw.
              -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top
                                      --   left corner of the clippng area.
              -> (JSNumber, JSNumber) -- ^ The width and height of the clipping area
              -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
              -> (JSNumber, JSNumber) -- ^ The width and height to scale the image to.
              -> Action JSObject ()
drawImageClip img (cx, cy) (cw, ch) (x,y) (w,h) =
  method "drawImage" ( img
                     , cx, cy
                     , cw, ch
                     , x, y
                     , w, h)

-- | Fills the current path with the current fill style.
fill :: Action JSObject ()
fill = method "fill" ()

-- | Draws a filled rectangle given by its top left corner and size with the
--   current fill style.
fillRect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectangle.
         -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
         -> Action JSObject ()
fillRect (x,y) (w,h) = method "fillRect" (x, y, w, h)

-- | Sets the fill style of the context. A color value of the form "#XXXXXX"
--   is expected.
setFillStyle :: JSString -> Action JSObject ()
-- TODO: Add support for gradients and patterns.
setFillStyle fs = "fillStyle" := fs

-- | Fills a text with the current fill style.
fillText :: JSString            -- ^ The text to fill.
         -> (JSNumber,JSNumber) -- ^ The x and y position of the
                                --   bottom left corner of the text.
         -> Action JSObject ()
fillText s (x,y) = method "fillText" (s, x, y)

-- | Fills a text with the current fill style.
fillText' :: JSString             -- ^ The text to fill.
          -> (JSNumber, JSNumber) -- ^ The x and y position of the
                                  --   bottom left corner of the text.
          -> JSNumber             -- ^ The maximum allowed width of the text.
          -> Action JSObject ()
fillText' s (x,y) maxW = method "fillText" (s, x, y, maxW)

-- | Sets the font used by the context.
setFont :: JSString -> Action JSObject ()
setFont f = "font" := f

-- | Get the image data of the specified rectanlge of the canvas.
getImageData :: (JSNumber, JSNumber)     -- ^ The x and y coordinate of the top left
                                         --   corner of the rectangle to extract.
             -> (JSNumber, JSNumber)     -- ^ The width and height of the rectangle.
             -> Action JSObject JSObject -- ^ Returns the image data object
                                         --   with the extracted information.
getImageData (x,y) (w,h) =
  method "getImageData" (x, y, w , h)

-- | Sets the global alpha value.
setGlobalAlpha :: JSNumber -> Action JSObject ()
setGlobalAlpha a = "globalAlpha" := a

-- | Returns true if the given point is in the path and false otherwise.
isPointInPath :: (JSNumber, JSNumber)   -- ^ The x and y coordinate of the point to check
              -> Action JSObject JSBool
isPointInPath (x,y) = method "isPointInPath" (x, y)

-- | Sets the line cap style to use.
--   Possible values are: "butt", "round", "square";
setLineCap :: JSString -> Action JSObject ()
setLineCap lc = "lineCap" := lc

-- | Sets the line join style to use.
--   Possible values are: "bevel", "round", "meter";
setLineJoin :: JSString -> Action JSObject ()
setLineJoin lj = "lineJoin" := lj

-- | Create a straight line path from the current point to the given point.
lineTo :: (JSNumber,JSNumber) -- ^ The x and y location the line is drawn to.
       -> Action JSObject ()
lineTo (x,y) = method "lineTo" (x, y)

-- | Sets the line width used when stroking.
setLineWidth :: JSNumber           -- ^ The line new line width in pixels.
          -> Action JSObject ()
setLineWidth lw = "lineWidth" := lw

-- | Returns the miter limit used when drawing a miter line join.
miterLimit :: JSSelector JSNumber
miterLimit = label $ string "miterLimit"

-- | Sets the miter limit used when drawing a miter line join.
setMiterLimit :: JSNumber           -- ^ The new miter limit.
              -> Action JSObject ()
setMiterLimit ml = "miterLimit" := ml

-- | Returns an object that contains the width of the specified text is pixels.
--   See 'width' selector.
measureText :: JSString                 -- ^ The text to be measured.
            -> Action JSObject JSObject
measureText s = method "measureText" (s)

-- | Move the path to the given location.
moveTo :: (JSNumber,JSNumber) -- ^ The new x and y location of the path.
       -> Action JSObject ()
moveTo (x,y) = method "moveTo" (x, y)

-- | Uses the given image data to replace the rectangle of the
--   canvas at the given position.
putImageData :: JSObject             -- ^ The new image data.
             -> (JSNumber, JSNumber) -- ^ The x and y coordinate of the top left corner.
             -> Action JSObject ()
putImageData imgData (x,y) = method "putImageData" (imgData, x, y)

-- | Creates a rectangle in the current context.
rect :: (JSNumber,JSNumber) -- ^ The top left corner of the rectangle.
     -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
     -> Action JSObject ()
rect (x,y) (w,h) = method "rect" (x, y, w, h)

-- | Restores the last saved paths and state of the context.
restore :: Action JSObject ()
restore = method "restore" ()

-- | Rotates the current drawing. The rotation will only affect drawings
--   made after the rotation.
rotate :: JSNumber           -- ^ The rotation angle in radians.
       -> Action JSObject ()
rotate a = method "rotate" (a)

-- | Scales the current drawing.
scale :: (JSNumber,JSNumber) -- ^ The factors to scale the
                             --   width and the height with.
      -> Action JSObject ()
scale (sw,sh) = method "scale" (sw, sh)

-- | Saves the state of the current context.
save :: Action JSObject ()
save = method "save" ()

-- | Resets the transformation matrix to identity and then applies
--   'transform' with the given paramters to it.
setTransform :: JSNumber -- ^ Scales the drawings horizontally.
             -> JSNumber -- ^ Skew the drawings horizontally.
             -> JSNumber -- ^ Skew the drawings vertically.
             -> JSNumber -- ^ Scales the drawings vertically.
             -> JSNumber -- ^ Moves the drawings horizontally.
             -> JSNumber -- ^ Moves the drawings vertically.
             -> Action JSObject ()
setTransform a b c d e f =
  method "setTransform" (a, b, c, d, e, f)

-- | Sets the shadow color property.
--   The given string has to be a valid CSS color value or a
--   color of the form '#XXXXXX'
setShadowColor :: JSString           -- ^ The color to use as shadow color.
            -> Action JSObject ()
setShadowColor c = "shadowColor" := c

-- | Sets the blur level for shadows.
setShadowBlur :: JSNumber           -- ^ The blur level for the shadow in pixels.
           -> Action JSObject ()
setShadowBlur b = "shadowBlur" := b

-- | Sets the x offset of a shadow from a shape.
setShadowOffsetX :: JSNumber           -- ^ The x offset of the shadow in pixels.
              -> Action JSObject ()
setShadowOffsetX x = "shadowOffsetX" := x

-- | Sets the y offset of a shadow from a shape.
setShadowOffsetY :: JSNumber           -- ^ The y offset of the shadow in pixels.
              -> Action JSObject ()
setShadowOffsetY y = "shadowOffsetY" := y

-- | Draws the current path using the current stroke style.
stroke :: Action JSObject ()
stroke = method "stroke" ()

-- | Strokes a rectanlge using the current stroke style.
strokeRect :: (JSNumber,JSNumber) -- ^ The x and y coordinate of the top left corner.
           -> (JSNumber,JSNumber) -- ^ The width and height of the rectangle.
           -> Action JSObject ()
strokeRect (x,y) (w,h) = method "strokeRect" (x, y, w, h)

-- | Strokes a text using the current stroke style.
strokeText :: JSString            -- ^ The text to stroke.
           -> (JSNumber,JSNumber) -- ^ The x and y coordinate to stroke the text at.
           -> Action JSObject ()
strokeText s (x,y) = method "strokeText" (s, x, y)

-- | Sets the stroke style of the context. A color value of the form "#XXXXXX"
--   is expected.
setStrokeStyle :: JSString -> Action JSObject ()
-- TODO: Add support for patterns and gradients.
setStrokeStyle c = "strokeStyle" := c

-- | Sets the text alignment to be used when drawing text.
--   Possible values are: "center", "end", "left", "right", "start"
setTextAlign :: JSString -> Action JSObject ()
setTextAlign ta = "textAlign" := ta

-- | Sets the baseline to use when drawing text.
--   Possible values are: "alphabetic", "top", "hanging", "middle", "ideographic", "bottom"
setTextBaseline :: JSString -> Action JSObject ()
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
          -> Action JSObject ()
transform a b c d e f =
  method "transform" (a, b, c, d, e, f)

-- | Translate the current drawing.
translate :: (JSNumber,JSNumber) -- ^ The x and y values to translate by.
          -> Action JSObject ()
translate (x,y) = method "translate" (x, y)

-- | Create a quadratic curve to extend the current path.
quadraticCurveTo :: (JSNumber, JSNumber) -- ^ The control point of the curve.
                 -> (JSNumber, JSNumber) -- ^ The endpoint of the curve.
                 -> Action JSObject ()
quadraticCurveTo (cx, cy) (ex, ey) =
  method "quadraticCurveTo" (cx, cy, ex, ey)

-- | Selects the width attribute.
width :: JSSelector JSNumber
width = attribute "width"

-- | Selects the height attribute.
height :: JSSelector JSNumber
height = attribute "height"

-- | Selects the data attribute.
data' :: JSSelector JSObject
data' = attribute "data"

-- | Selects the global alpha attribute.
globalAlpha :: JSSelector JSNumber
globalAlpha = attribute "globalAlpha"

-- | Selects the shadow color attribute.
shadowColor :: JSSelector JSString
shadowColor = attribute "shadowColor"

-- | Selects the blur level for shadows.
shadowBlur :: JSSelector JSNumber
shadowBlur = attribute "shadowBlur"

-- | Selects the x offset of a shadow from a shape.
shadowOffsetX :: JSSelector JSNumber
shadowOffsetX = attribute "shadowOffsetX"

-- | Selects the y offset of a shadow from a shape.
shadowOffsetY :: JSSelector JSNumber
shadowOffsetY = attribute "shadowOffsetY"

-- | Selects the stroke style of the context.
strokeStyle :: JSSelector JSString
-- TODO: Add support for patterns and gradients.
strokeStyle = attribute "strokeStyle"

-- | Selects the text alignment to be used when drawing text.
--   Possible values are: "center", "end", "left", "right", "start"
textAlign :: JSSelector JSString
textAlign = attribute "textAlign"

-- | Selects the baseline to use when drawing text.
--   Possible values are: "alphabetic", "top", "hanging", "middle", "ideographic", "bottom"
textBaseline :: JSSelector JSString
textBaseline = attribute "textBaseline"

-- | Sets the line cap style to use.
--   Possible values are: "butt", "round", "square";
lineCap :: JSSelector JSString
lineCap = attribute "lineCap"

-- | Selects the line join style to use.
--   Possible values are: "bevel", "round", "meter";
lineJoin :: JSSelector JSString
lineJoin = attribute "lineJoin"

-- | Selects the line width used when stroking.
lineWidth :: JSSelector JSNumber
lineWidth = attribute "lineWidth"

-- | Selects the font used by the context.
font :: JSSelector JSString
font = attribute "font"
