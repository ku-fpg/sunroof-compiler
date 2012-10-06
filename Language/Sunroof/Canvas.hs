{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies #-}

module Language.Sunroof.Canvas where

import Language.Sunroof.Types

-- TODO: not sure about the string => JSSelector (JSFunction a) overloading.

arc :: (JSValue,JSValue,JSValue,JSValue,JSValue,JSBool) -> Action JSObject ()
arc (a,b,c,d,e,f) = method "arc" [cast a,cast b,cast c, cast d, cast e,cast f]

beginPath :: () -> Action JSObject ()
beginPath () = method "beginPath" []

bezierCurveTo :: (JSNumber,JSNumber,JSNumber,JSNumber,JSNumber,JSNumber) -> Action JSObject ()
bezierCurveTo (a,b,c,d,e,f) = method "bezierCurveTo" [cast a, cast b, cast c, cast d, cast e, cast f]

clearRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> Action JSObject ()
clearRect (a,b,c,d) = method "clearRect" [cast a, cast b, cast c, cast d]

closePath :: () -> Action JSObject ()
closePath () = method "closePath" []

fill :: () -> Action JSObject ()
fill () = method "fill" []

fillRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> Action JSObject ()
fillRect (a,b,c,d) = method "fillRect" [cast a, cast b, cast c, cast d]

fillStyle :: JSString -> Action JSObject ()
fillStyle a = "fillStyle" := a

fillText :: (JSString,JSNumber,JSNumber) -> Action JSObject ()
fillText (a,b,c) = method "fillText" [cast a, cast b, cast c]

font :: JSString -> Action JSObject ()
font a = "font" := a

globalAlpha :: JSNumber -> Action JSObject ()
globalAlpha a = "globalAlpha" := a

lineCap :: JSString -> Action JSObject ()
lineCap a = "lineCap" := a

lineJoin :: JSString -> Action JSObject ()
lineJoin a = "lineJoin" := a

lineTo :: (JSNumber,JSNumber) -> Action JSObject ()
lineTo (a,b) = method "lineTo" [cast a, cast b]

lineWidth :: JSNumber -> Action JSObject ()
lineWidth a = "lineWidth" := a

miterLimit :: JSNumber -> Action JSObject ()
miterLimit a = "miterLimit" := a

moveTo :: (JSNumber,JSNumber) -> Action JSObject ()
moveTo (a,b) = method "moveTo" [cast a, cast b]

restore :: () -> Action JSObject ()
restore () = method "restore" []

rotate :: JSNumber -> Action JSObject ()
rotate a = method "rotate" [cast a]

scale :: (JSNumber,JSNumber) -> Action JSObject ()
scale (a,b) = method "scale" [cast a, cast b]

save :: () -> Action JSObject ()
save () = method "save" []

stroke :: () -> Action JSObject ()
stroke () = method "stroke" []

strokeRect :: (JSNumber,JSNumber,JSNumber,JSNumber) -> Action JSObject ()
strokeRect (a,b,c,d) = method "strokeRect" [cast a,cast b,cast c, cast d]

strokeText :: (JSString,JSNumber,JSNumber) -> Action JSObject ()
strokeText (a,b,c) = method "strokeText" [cast a,cast b,cast c]

strokeStyle :: JSString -> Action JSObject ()
strokeStyle a = "strokeStyle" := a

textAlign :: JSString -> Action JSObject ()
textAlign a = "textAlign" := a

textBaseline :: JSString -> Action JSObject ()
textBaseline a = "textBaseline" := a

transform :: (JSNumber,JSNumber,JSNumber,JSNumber,JSNumber,JSNumber) -> Action JSObject ()
transform (a,b,c,d,e,f) = method "transform" [cast a,cast b,cast c, cast d, cast e,cast f]

translate :: (JSNumber,JSNumber) -> Action JSObject ()
translate (a,b) = method "translate" [cast a,cast b]
