{-# LANGUAGE DataKinds #-}

module Language.Sunroof.JS.JQuery
  (
  -- * General JQuery API
    dollar
  , jQuery, jq
  -- * DOM API
  , append
  , html
  , text
  , on
  , addClass
  ) where

import Language.Sunroof.Classes
  ( JSArgument(..)
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
-- DOM API
-- -----------------------------------------------------------------------

-- | See <http://api.jquery.com/append/>.
append :: JSObject -> JSObject -> JS t ()
append x = invoke "append" x

html :: JSString -> JSObject -> JS t JSObject
html nm = invoke "html"  nm

text :: JSString -> JSObject -> JS t JSObject
text = invoke "text"

on :: (JSArgument a) => JSString -> JSString -> (a -> JS A ()) -> JSObject -> JS t ()
on nm sel f o = do
     callback <- function f
     o # invoke "on" (nm,sel,callback)

addClass :: JSString -> JSObject -> JS t ()
addClass = invoke "addClass"
