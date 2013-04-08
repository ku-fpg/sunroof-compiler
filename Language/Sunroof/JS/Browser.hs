{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Sunroof.JS.Browser
-- Copyright   :  (c) 2011 The University of Kansas
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  ???
--
-- A reflection of the standard browser Javascript API.
--
module Language.Sunroof.JS.Browser
  ( -- * Top level functions
    alert
  , confirm
  , prompt
  , decodeURI
  , encodeURI
  , decodeURIComponent
  , encodeURIComponent
  , eval
  , isFinite
  , isNaN
  , parseFloat
  , parseInt
  -- * Window API
  , window
  , setInterval
  , clearInterval
  , setTimeout
  , clearTimeout
  -- * Screen API
  , screen
  -- * Document API
  , document
  , getElementById
  , getElementsByName
  , getElementsByTagName
  , createAttribute
  , createElement
  , createTextNode
  , open
  , close
  , write
  , writeln
  , setCookie
  , cookie
  , referrer
  , setTitle
  , title
  , url
  -- * Image DOM
  , src
  -- * The JavaScript Console
  , JSConsole
  , console
  , Language.Sunroof.JS.Browser.log
  , debug
  , info
  , warn
  , Language.Sunroof.JS.Browser.error
  ) where

import Prelude hiding (isNaN)

import Data.Boolean ( BooleanOf, IfB(..), EqB(..) )

import Language.Sunroof.Types
  ( JS, JSB
  , JS ( (:=) )
  , fun
  , invoke
  , attr
  , apply
  , (#)
  , continuation
  )
import Language.Sunroof.Classes ( Sunroof(..), SunroofArgument )
import Language.Sunroof.Selector ( JSSelector )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Object ( JSObject, object )
import Language.Sunroof.JS.String ( JSString )
import Language.Sunroof.JS.Number ( JSNumber )

-- -----------------------------------------------------------------------
-- Object Independent Functions
-- -----------------------------------------------------------------------

-- | Display the given text in a message box.
--
--   See <http://www.w3schools.com/js/js_popup.asp>.
alert :: JSString -> JS t ()
alert msg = fun "alert" `apply` (msg)

-- | Ask the user to confirm the given massege.
--
--   See <http://www.w3schools.com/js/js_popup.asp>.
confirm :: JSString -> JS t JSBool
confirm msg = fun "confirm" `apply` (msg)

-- | Ask the user to give some input. May return @null@
--   or a string. Don't forget to check against 'nullJS'
--   before casting to string.
--
--   See <http://www.w3schools.com/js/js_popup.asp>.
prompt :: JSString -> JSString -> JS t JSObject
prompt msg val = fun "prompt" `apply` (msg, val)

-- | Decode the URI encoded in the given string.
decodeURI :: JSString -> JS t JSString
decodeURI str = fun "decodeURI" `apply` (str)

-- | Encode the given string in URI encoding.
encodeURI :: JSString -> JS t JSString
encodeURI str = fun "encodeURI" `apply` (str)

-- | Decode the URI encoded string. For use with 'encodeURIComponent'.
decodeURIComponent :: JSString -> JS t JSString
decodeURIComponent str = fun "decodeURIComponent" `apply` (str)

-- | Encode the string with URI encoding. This encodes a few more
--   characters to make the string safe for direct server communication (AJAX).
encodeURIComponent :: JSString -> JS t JSString
encodeURIComponent str = fun "encodeURIComponent" `apply` (str)

-- | Evaluate the given JavaScript string if possible. Returns
--   the result of evaluation.
-- TODO: think about this a bit.
eval :: (Sunroof a) => JSString -> JS t a
eval str = fun "eval" `apply` (str)

-- | Check if a given number is within the valid JavaScript number range.
isFinite :: JSNumber -> JS t JSBool
isFinite n = fun "isFinite" `apply` (n)

-- | Check if a given number is NaN or not.
isNaN :: JSNumber -> JS t JSBool
isNaN n = fun "isNaN" `apply` (n)

-- | Parse the given string to a number.
parseFloat :: JSString -> JS t JSNumber
parseFloat str = fun "parseFloat" `apply` (str)

-- | Parse the given string to a number.
parseInt :: JSString -> JS t JSNumber
parseInt str = fun "parseInt" `apply` (str)

-- -----------------------------------------------------------------------
-- Window API
-- -----------------------------------------------------------------------

-- | The window object.
window :: JSObject
window = object "window"

-- | Calls a function at specified intervals in milliseconds.
--   It will continue calling the function until 'clearInterval' is called,
--   or the window is closed. The returned number is needed for 'clearInterval'.
--   This is supposed to be called on the 'window' object.
--   See: <http://www.w3schools.com/jsref/met_win_setinterval.asp>
setInterval :: (() -> JSB ()) -> JSNumber -> JSObject -> JS t JSNumber
setInterval f interval o = do
  callback <- continuation f
  o # invoke "setInterval" (callback, interval)

-- | Clears a timer set with the 'setInterval' method.
--   This is supposed to be called on the 'window' object.
--   See: <http://www.w3schools.com/jsref/met_win_clearinterval.asp>
clearInterval :: JSNumber -> JSObject -> JS t ()
clearInterval ident = invoke "clearInterval" (ident)

-- | Execute the given continutation after the given amount of
--   milliseconds. Returns a handler for the set timer.
--   This is supposed to be called on the 'window' object.
--   See: <http://www.w3schools.com/jsref/met_win_settimeout.asp>
setTimeout :: (() -> JSB ()) -> JSNumber -> JSObject -> JS t JSNumber
setTimeout f interval o = do
  callback <- continuation f
  o # invoke "setTimeout" (callback, interval)

-- | Removes the timer associated with the given handler.
--   This is supposed to be called on the 'window' object.
--   See: <http://www.w3schools.com/jsref/met_win_cleartimeout.asp>
clearTimeout :: JSNumber -> JSObject -> JS t ()
clearTimeout ident = invoke "clearTimeout" (ident)

-- -----------------------------------------------------------------------
-- Screen API
-- -----------------------------------------------------------------------

-- | The screen object.
screen :: JSObject
screen = object "screen"

-- -----------------------------------------------------------------------
-- Document API
-- -----------------------------------------------------------------------

-- | The document object.
document :: JSObject
document = object "document"

-- | Get the DOM object of the element with the given id.
--   For use with 'document'.
getElementById :: JSString -- ^ The id.
               -> JSObject -> JS t JSObject
getElementById ident = invoke "getElementById" (ident)

-- | Get the DOM objects of the elements with the given name.
--   For use with 'document'.
getElementsByName :: JSString -- ^ The name.
                  -> JSObject -> JS t JSObject
getElementsByName name = invoke "getElementsByName" (name)

-- | Get the DOM objects of the elements with the given tag.
--   For use with 'document'.
getElementsByTagName :: JSString -- ^ The tag name.
                     -> JSObject -> JS t JSObject
getElementsByTagName tag = invoke "getElementsByTagName" (tag)

-- | Create a attribute DOM node with the given name.
--   For use with 'document'.
createAttribute :: JSString -- ^ The name of the new attribute.
                -> JSObject -> JS t JSObject
createAttribute a = invoke "createAttribute" (a)

-- | Create a element DOM node with the given tag name.
--   For use with 'document'.
createElement :: JSString -- ^ The tag name of the new element.
              -> JSObject -> JS t JSObject
createElement e = invoke "createElement" (e)

-- | Create a text DOM node with the given string as text.
--   For use with 'document'.
createTextNode :: JSString -- ^ The text of the new text node.
               -> JSObject -> JS t JSObject
createTextNode text = invoke "createTextNode" (text)

-- | Opens the document for writing.
--   For use with 'document'.
open :: JSObject -> JS t ()
open = invoke "open" ()

-- | Closes the document after writing.
--   For use with 'document'.
close :: JSObject -> JS t ()
close = invoke "close" ()

-- | Writes something into the document.
--   For use with 'document'.
write :: JSString -> JSObject -> JS t ()
write str = invoke "write" (str)

-- | Write something into the document and appends a new line.
--   For use with 'document'.
writeln :: JSString -> JSObject -> JS t ()
writeln str = invoke "writeln" (str)

-- | Sets the value of the cookie.
--   For use with 'document'.
setCookie :: JSString -> JSObject -> JS t ()
setCookie c = "cookie" := c

-- | Returns the value of the cookie.
--   For use with 'document'.
cookie :: JSSelector JSString
cookie = attr "cookie"

-- | Returns the referrer of the document.
--   For use with 'document'.
referrer :: JSSelector JSString
referrer = attr "referrer"

-- | Sets the title of the document.
--   For use with 'document'.
setTitle :: JSString -> JSObject -> JS t ()
setTitle t = "title" := t

-- | Returns the title of the document.
--   For use with 'document'.
title :: JSSelector JSString
title = attr "title"

-- | Returns the complete URL of the document.
--   For use with 'document'.
url :: JSSelector JSString
url = attr "URL"

-- | Returns the src of a DOM image object.
src :: JSSelector JSString
src = attr "src"

-- -----------------------------------------------------------------------
-- Console API
-- -----------------------------------------------------------------------

-- | The type of the debugging console object.
--   See:
--   <https://developers.google.com/chrome-developer-tools/docs/console-api>,
--   <https://developer.mozilla.org/en-US/docs/DOM/console>,
--   <http://msdn.microsoft.com/en-us/library/windows/apps/hh696634.aspx>;
data JSConsole = JSConsole JSObject

-- | Show the Javascript.
instance Show JSConsole where
  show (JSConsole o) = show o

-- | First-class values in Javascript.
instance Sunroof JSConsole where
        box = JSConsole . box
        unbox (JSConsole e) = unbox e

-- | Associated boolean is 'JSBool'.
type instance BooleanOf JSConsole = JSBool

-- | Can be returned in branches.
instance IfB JSConsole where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSConsole where
  (JSConsole a) ==* (JSConsole b) = a ==* b

-- | The console object.
console :: JSConsole
console = JSConsole (object "console")

-- | Log the given message.
log :: (SunroofArgument a) => a -> JSConsole -> JS t ()
log a (JSConsole o) = o # invoke "log" a

-- | Send a debug level message to the console.
debug :: (SunroofArgument a) => a -> JSConsole -> JS t ()
debug a (JSConsole o) = o # invoke "debug" a

-- | Send a info message to the console.
info :: (SunroofArgument a) => a -> JSConsole -> JS t ()
info a (JSConsole o) = o # invoke "info" a

-- | Send a warning message to the console.
warn :: (SunroofArgument a) => a -> JSConsole -> JS t ()
warn a (JSConsole o) = o # invoke "warn" a

-- | Send an error message to the console.
error :: (SunroofArgument a) => a -> JSConsole -> JS t ()
error a (JSConsole o) = o # invoke "error" a


