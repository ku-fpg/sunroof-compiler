{-# LANGUAGE DataKinds #-}

-- | This module provides parts of the JQuery API (<http://api.jquery.com/>).
module Language.Sunroof.JS.JQuery
  (
  -- * General JQuery API
    dollar
  , jQuery, jq
  -- * Manipulation > DOM
  , append
  , html, setHtml
  , text, setText
  -- * Event Handling
  , on
  -- * Manipulation > Class Attribute
  , addClass
  -- * Manipulation > Style Properties
  , innerWidth
  , innerHeight
  ) where

import Language.Sunroof.Classes
  ( SunroofArgument(..)
  )
import Language.Sunroof.Types
  ( JSFunction
  , JS
  , fun
  , invoke
  , apply
  , (#)
  , function
  , T(..)
  )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.String ( JSString )
import Language.Sunroof.JS.Number ( JSNumber )

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
-- Event Handling
-- -------------------------------------------------------------

-- | See <http://api.jquery.com/on/>.
on :: (SunroofArgument a) => JSString -> JSString -> (a -> JS A ()) -> JSObject -> JS t ()
on nm sel f o = do
     callback <- function f
     o # invoke "on" (nm,sel,callback)

-- -------------------------------------------------------------
-- Manipulation > Class Attribute
-- -------------------------------------------------------------

-- | See <http://api.jquery.com/addClass/>.
addClass :: JSString -> JSObject -> JS t ()
addClass = invoke "addClass"

-- -------------------------------------------------------------
-- Manipulation > Style Properties
-- -------------------------------------------------------------

-- | See <http://api.jquery.com/innerHeight/>.
innerWidth :: JSObject -> JS t JSNumber
innerWidth = invoke "innerWidth" ()

-- | See <http://api.jquery.com/innerWidth/>.
innerHeight :: JSObject -> JS t JSNumber
innerHeight = invoke "innerHeight" ()