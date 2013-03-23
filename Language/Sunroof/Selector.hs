
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'JSSelector' are used to access fields of Javascript objects.
module Language.Sunroof.Selector
  ( JSSelector
  , label, index
  , unboxSelector
  , (!)
  ) where

import Data.String ( IsString(..) )
import Data.Proxy ( Proxy(Proxy) )

import Language.Sunroof.JavaScript ( Expr, E(Dot), ExprE(ExprE) )
import Language.Sunroof.Classes ( Sunroof(..) )
import Language.Sunroof.JS.String ( JSString, string )
import Language.Sunroof.JS.Number ( JSNumber )

-- | A 'JSSelector' selects a field or attribute from a Javascript object.
--   The phantom type is the type of the selected value. Note the selected 
--   field or attributes may also array entries ('index').
data JSSelector a = JSSelector Expr

-- | Selectors can be created from the name of their attribute.
instance IsString (JSSelector a) where
  fromString = JSSelector . unbox . string

instance Show (JSSelector a) where
  show (JSSelector ids) = show ids

-- | Create a selector for a named field or attribute.
--   For type safty it is adivsed to use this with an 
--   accompanying type signature. Example:
--   
-- > array ! label "length"
--   
--   See '!' for further information on usage.
label :: JSString -> JSSelector a
label = JSSelector . unbox

-- | Create a selector for an indexed value (e.g. array access).
--   For type safty it is adivsed to use this with an 
--   accompanying type signature. Example:
--   
-- > array ! index 4
--   
--   See '!' for further information on usage.
index :: JSNumber -> JSSelector a
index = JSSelector . unbox

-- | Provided for internal usage by the compiler. Unwraps the 
--   selector.
unboxSelector :: JSSelector a -> Expr
unboxSelector (JSSelector e) = e

---------------------------------------------------------------

infixl 1 !

-- | Operator to use a selector on a Javascript object. Examples:
--   
-- > array ! label "length"
-- > array ! index 4
(!) :: forall o a . (Sunroof o, Sunroof a) => o -> JSSelector a -> a
(!) arr (JSSelector idx) = box $ Dot (ExprE $ unbox arr) 
                                     (ExprE $ idx) 
                                     (typeOf (Proxy :: Proxy a))


