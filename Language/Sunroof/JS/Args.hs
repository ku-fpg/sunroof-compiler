
{-# LANGUAGE TypeFamilies #-}

-- | Strings in Javascript.
module Language.Sunroof.JS.Args
  ( JSArgs
  , toJSArgs
  ) where

import Data.Boolean

import Language.Sunroof.JavaScript
import Language.Sunroof.Classes
import Language.Sunroof.Types
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

newtype JSArgs a = JSArgs Expr

instance Show (JSArgs a) where
   show (JSArgs e) = showExpr False e

-- | Arrays are first-class Javascript values.
instance (SunroofArgument a) => Sunroof (JSArgs a) where
  box = JSArgs
  unbox (JSArgs e) = e

-- | The boolean of arrays are 'JSBool'.
type instance BooleanOf (JSArgs a) = JSBool

-- | You can write branches that return arrays.
instance (SunroofArgument a) => IfB (JSArgs a) where
  ifB = jsIfB

-- creates a @JSArgs@ array that contains the arguments.
toJSArgs :: (SunroofArgument a) => a -> JS t (JSArgs a)
toJSArgs args = fun "[]" $$ args

