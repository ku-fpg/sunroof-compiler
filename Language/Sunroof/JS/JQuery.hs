
module Language.Sunroof.JS.JQuery 
  -- General JQuery API
  ( dollar
  , jQuery, jq
  -- DOM API
  , append
  , html
  ) where

import Language.Sunroof.Types
  ( JSString
  , JSObject
  , JSFunction
  , JS
  , Action
  , call
  , method
  , apply
  )

-- -----------------------------------------------------------------------
-- JQuery interface
-- -----------------------------------------------------------------------

-- | The dollar function.
--   See <http://api.jquery.com/jQuery/>.
dollar :: JSFunction JSString JSObject
dollar = call "$"

-- | Calls the JQuery dollar function.
--   See <http://api.jquery.com/jQuery/>.
jQuery :: JSString -> JS JSObject
jQuery nm = dollar `apply` nm

-- | Short-hand for 'jQuery'.
jq :: JSString -> JS JSObject
jq = jQuery

-- -----------------------------------------------------------------------
-- DOM API
-- -----------------------------------------------------------------------

-- | See <http://api.jquery.com/append/>.
append :: JSObject -> Action JSObject ()
append x = method "append" x

html :: JSString -> Action JSObject JSObject
html nm = method "html"  nm