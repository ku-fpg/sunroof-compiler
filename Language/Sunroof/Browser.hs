{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Sunroof.Browser 
  -- Top level functions
  ( alert
  , decodeURI
  , encodeURI
  , decodeURIComponent
  , encodeURIComponent
  , eval
  , isFinite
  , isNaN
  , parseFloat
  , parseInt
  -- Document functions
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
  ) where

import Prelude hiding (isNaN)

import Language.Sunroof.Types 
  ( JSString, JSNumber, JSObject, JSBool, JSValue
  , JS, JSSelector
  , Action(..)
  , call
  , with
  , cast
  , method
  , object
  , attribute
  , (<$>)
  )

-- -----------------------------------------------------------------------
-- Object Independent Functions
-- -----------------------------------------------------------------------

-- | Display the given text in a message box.
alert :: JSString -> JS ()
alert msg = call "alert" <$> with [cast msg]

-- | Decode the URI encoded in the given string.
decodeURI :: JSString -> JS JSString
decodeURI str = call "decodeURI" <$> with [cast str]

-- | Encode the given string in URI encoding.
encodeURI :: JSString -> JS JSString
encodeURI str = call "encodeURI" <$> with [cast str]

-- | Decode the URI encoded string. For use with 'encodeURIComponent'.
decodeURIComponent :: JSString -> JS JSString
decodeURIComponent str = call "decodeURIComponent" <$> with [cast str]

-- | Encode the string with URI encoding. This encodes a few more
--   characters to make the string safe for direct server communication (AJAX).
encodeURIComponent :: JSString -> JS JSString
encodeURIComponent str = call "encodeURIComponent" <$> with [cast str]

-- | Evaluate the given JavaScript string if possible. Returns 
--   the result of evaluation.
eval :: JSString -> JS JSValue
eval str = call "eval" <$> with [cast str]

-- | Check if a given number is within the valid JavaScript number range.
isFinite :: JSNumber -> JS JSBool
isFinite n = call "isFinite" <$> with [cast n]

-- | Check if a given number is NaN or not.
isNaN :: JSNumber -> JS JSBool
isNaN n = call "isNaN" <$> with [cast n]

-- | Parse the given string to a number.
parseFloat :: JSString -> JS JSNumber
parseFloat str = call "parseFloat" <$> with [cast str]

-- | Parse the given string to a number.
parseInt :: JSString -> JS JSNumber
parseInt str = call "parseInt" <$> with [cast str]

-- -----------------------------------------------------------------------
-- Document API
-- -----------------------------------------------------------------------

-- | The document object.
document :: JSObject
document = object "document"

-- | Get the DOM object of the element with the given id.
--   For use with 'document'.
getElementById :: JSString -- ^ The id.
               -> Action JSObject JSObject
getElementById ident = method "getElementById" [cast ident]

-- | Get the DOM objects of the elements with the given name.
--   For use with 'document'.
getElementsByName :: JSString -- ^ The name.
                  -> Action JSObject JSObject
getElementsByName name = method "getElementsByName" [cast name]

-- | Get the DOM objects of the elements with the given tag.
--   For use with 'document'.
getElementsByTagName :: JSString -- ^ The tag name.
                     -> Action JSObject JSObject
getElementsByTagName tag = method "getElementsByTagName" [cast tag]

-- | Create a attribute DOM node with the given name.
--   For use with 'document'.
createAttribute :: JSString -- ^ The name of the new attribute.
                -> Action JSObject JSObject
createAttribute attr = method "createAttribute" [cast attr]

-- | Create a element DOM node with the given tag name.
--   For use with 'document'.
createElement :: JSString -- ^ The tag name of the new element.
                -> Action JSObject JSObject
createElement e = method "createElement" [cast e]

-- | Create a text DOM node with the given string as text.
--   For use with 'document'.
createTextNode :: JSString -- ^ The text of the new text node.
                -> Action JSObject JSObject
createTextNode text = method "createTextNode" [cast text]

-- | Opens the document for writing.
--   For use with 'document'.
open :: Action JSObject ()
open = method "open" []

-- | Closes the document after writing.
--   For use with 'document'.
close :: Action JSObject ()
close = method "close" []

-- | Writes something into the document.
--   For use with 'document'.
write :: JSString -> Action JSObject ()
write str = method "write" [cast str]

-- | Write something into the document and appends a new line.
--   For use with 'document'.
writeln :: JSString -> Action JSObject ()
writeln str = method "writeln" [cast str]

-- | Sets the value of the cookie.
--   For use with 'document'.
setCookie :: JSString -> Action JSObject ()
setCookie c = "cookie" := c

-- | Returns the value of the cookie.
--   For use with 'document'.
cookie :: JSSelector JSString
cookie = attribute "cookie"

-- | Returns the referrer of the document.
--   For use with 'document'.
referrer :: JSSelector JSString
referrer = attribute "referrer"

-- | Sets the title of the document.
--   For use with 'document'.
setTitle :: JSString -> Action JSObject ()
setTitle t = "title" := t

-- | Returns the title of the document.
--   For use with 'document'.
title :: JSSelector JSString
title = attribute "title"

-- | Returns the complete URL of the document.
--   For use with 'document'.
url :: JSSelector JSString
url = attribute "URL"
