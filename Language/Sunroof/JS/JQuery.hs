{-# LANGUAGE DataKinds #-}

-- | This module provides parts of the JQuery API (<http://api.jquery.com/>).
module Language.Sunroof.JS.JQuery
  (
  -- * General JQuery API
    dollar
  , jQuery, jq
  -- * DOM
  , append
  , html, setHtml
  , text, setText
  -- * CSS
  , css, setCss
  , addClass, removeClass
  -- * Attributes
  , attribute, attr'
  , setAttr
  , removeAttr
  -- * Event Handling
  , on
  -- * Manipulation
  , innerWidth
  , innerHeight
  , outerWidth, outerWidth'
  , outerHeight, outerHeight'
  , clone, clone'
  ) where

import Language.Sunroof.Classes
  ( SunroofArgument(..)
  )
import Language.Sunroof.Types

import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.String ( JSString )
import Language.Sunroof.JS.Number ( JSNumber )
import Language.Sunroof.JS.Bool ( JSBool )

-- -----------------------------------------------------------------------
-- JQuery interface
-- -----------------------------------------------------------------------

-- | The dollar function.
--   See <http://api.jquery.com/jQuery/>.
dollar :: JSFunction JSString JSObject
dollar = fun "$"

-- | Calls the JQuery dollar function.
--   See <http://api.jquery.com/jQuery/>.
jQuery :: JSString -> JS t JSObject
jQuery nm = dollar `apply` nm

-- | Short-hand for 'jQuery'.
jq :: JSString -> JS t JSObject
jq = jQuery

-- -----------------------------------------------------------------------
-- Manipulation > DOM
-- -----------------------------------------------------------------------

-- | See <http://api.jquery.com/append/>.
append :: JSObject -> JSObject -> JS t ()
append x = invoke "append" x

-- | See @.html()@ at <http://api.jquery.com/html/>.
html :: JSObject -> JS t JSObject
html = invoke "html" ()

-- | See @.html(htmlString)@ at <http://api.jquery.com/html/>.
setHtml :: JSString -> JSObject -> JS t JSObject
setHtml s = invoke "html" s

-- | See @.text()@ at <http://api.jquery.com/text/>.
text :: JSObject -> JS t JSObject
text = invoke "text" ()

-- | See @.text(textString)@ at <http://api.jquery.com/text/>.
setText :: JSString -> JSObject -> JS t JSObject
setText s = invoke "text" s

-- -------------------------------------------------------------
-- CSS
-- -------------------------------------------------------------

-- | See @.css(propertyName)@ at <http://api.jquery.com/css/>.
css :: JSString -> JSObject -> JS t JSString
css prop = invoke "css" prop

-- | See @.css(propertyName, value)@ at <http://api.jquery.com/css/>.
setCss :: JSString -> JSString -> JSObject -> JS t JSString
setCss prop v = invoke "css" (prop, v)

-- | See <http://api.jquery.com/addClass/>.
addClass :: JSString -> JSObject -> JS t ()
addClass = invoke "addClass"

-- | See <http://api.jquery.com/removeClass/>.
removeClass :: JSString -> JSObject -> JS t ()
removeClass = invoke "removeClass"

-- -------------------------------------------------------------
-- Attributes
-- -------------------------------------------------------------

-- | See @.attr(attributeName)@ at <http://api.jquery.com/attr/>.
--   This binding does not have the original Javascript name,
--   because of the 'attr' function.
attribute :: JSString -> JSObject -> JS t JSString
attribute a = invoke "attr" a

-- | See @.attr(attributeName)@ at <http://api.jquery.com/attr/>.
--   This binding does not have the original Javascript name,
--   because of the 'attr' function.
attr' :: JSString -> JSObject -> JS t JSString
attr' = attribute

-- | See @.attr(attributeName, value)@ at <http://api.jquery.com/attr/>.
setAttr :: JSString -> JSString -> JSObject -> JS t JSString
setAttr a v = invoke "attr" (a, v)

-- | See: <http://api.jquery.com/removeAttr/>
removeAttr :: JSString -> JSObject -> JS t JSObject
removeAttr attrName = invoke "removeAttr" attrName

-- -------------------------------------------------------------
-- Event Handling
-- -------------------------------------------------------------

-- | See <http://api.jquery.com/on/>.
on :: (SunroofArgument a) => JSString -> JSString -> (a -> JS B ()) -> JSObject -> JS t ()
on nm sel f o = do
     callback <- continuation f
     o # invoke "on" (nm,sel,callback)

-- -------------------------------------------------------------
-- Manipulation > Style Properties
-- -------------------------------------------------------------

-- | See <http://api.jquery.com/innerHeight/>.
innerWidth :: JSObject -> JS t JSNumber
innerWidth = invoke "innerWidth" ()

-- | See <http://api.jquery.com/innerWidth/>.
innerHeight :: JSObject -> JS t JSNumber
innerHeight = invoke "innerHeight" ()

-- | See <http://api.jquery.com/outerWidth/>.
outerWidth :: JSObject -> JS t JSNumber
outerWidth = invoke "outerWidth" ()

-- | See <http://api.jquery.com/outerWidth/>.
outerWidth' :: JSBool -> JSObject -> JS t JSNumber
outerWidth' includeMargin = invoke "outerWidth" includeMargin

-- | See <http://api.jquery.com/outerHeight/>.
outerHeight :: JSObject -> JS t JSNumber
outerHeight = invoke "outerHeight" ()

-- | See <http://api.jquery.com/outerHeight/>.
outerHeight' :: JSBool -> JSObject -> JS t JSNumber
outerHeight' includeMargin = invoke "outerHeight" includeMargin

-- | See @.clone()@ at <http://api.jquery.com/clone/>.
clone :: JSObject -> JS t JSObject
clone = invoke "clone" ()

-- | See @.clone(withDataAndEvents, deepWithDataAndEvents)@ at <http://api.jquery.com/clone/>.
clone' :: JSBool -> JSBool -> JSObject -> JS t JSObject
clone' withDataAndEvents deepWithDataAndEvents =
  invoke "clone" (withDataAndEvents, deepWithDataAndEvents)


