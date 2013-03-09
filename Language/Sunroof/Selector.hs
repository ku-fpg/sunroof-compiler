
{-# LANGUAGE ScopedTypeVariables #-}

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
import Language.Sunroof.JS.Object ( JSObject )

-- | a 'JSSelector' selects a field from a JSObject.
-- The phantom is the type of the selected value.
{-data JSSelector :: * -> * where
        JSSelector :: JSString           -> JSSelector a-}
data JSSelector a = JSSelector Expr

instance IsString (JSSelector a) where
  fromString = JSSelector . unbox . string

instance Show (JSSelector a) where
  show (JSSelector ids) = show ids


label :: JSString -> JSSelector a
label = JSSelector . unbox

index :: JSNumber -> JSSelector a
index = JSSelector . unbox

unboxSelector :: JSSelector a -> Expr
unboxSelector (JSSelector e) = e

---------------------------------------------------------------

infixl 1 !

(!) :: forall a . (Sunroof a) => JSObject -> JSSelector a -> a
(!) arr (JSSelector idx) = box $ Dot (ExprE $ unbox arr) (ExprE $ idx) (typeOf (Proxy :: Proxy a))