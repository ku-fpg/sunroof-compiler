
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Sunroof.JS.Array
  ( JSArray
  , array, newArray
  , length'
  , push, pop
  , shift, unshift
  , forEach
  ) where

import Prelude hiding ( lookup, length )

import Data.List ( intercalate )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Boolean ( BooleanOf, IfB(..) )

import Language.Sunroof.JavaScript ( Expr, showExpr, literal, binOp )
import Language.Sunroof.Types 
  ( JS, T(A)
  , cast, invoke, function, attr, new
  , (#) )
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..), SunroofArgument(..) )
import Language.Sunroof.Selector ( JSSelector )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Number ( JSNumber )

-- -------------------------------------------------------------
-- JSArray Type
-- -------------------------------------------------------------

data JSArray a = JSArray Expr

instance Show (JSArray a) where
  show (JSArray v) = showExpr False v

instance (Sunroof a) => Sunroof (JSArray a) where
  box = JSArray
  unbox (JSArray o) = o

type instance BooleanOf (JSArray a) = JSBool

instance (Sunroof a) => IfB (JSArray a) where
  ifB = jsIfB

{- This conflicts with the string instance.
instance (SunroofValue a) => SunroofValue [a] where
  type ValueOf [a] = JSArray (ValueOf a)
  -- Uses JSON
  js l  = JSArray $ literal $ "[" ++ intercalate "," (fmap (showVar . js) l) ++ "]"
-}

instance (Sunroof a) => Monoid (JSArray a) where
  mempty = emptyArray
  mappend (JSArray e1) (JSArray e2) = box $ binOp "[].concat" e1 e2

instance (Sunroof a) => Semigroup (JSArray a) where
  (<>) = mappend

-- -------------------------------------------------------------
-- JSArray Combinators
-- -------------------------------------------------------------

array :: (SunroofValue a, Sunroof (ValueOf a)) => [a] -> JSArray (ValueOf a)
array l  = box $ literal $ "[" ++ intercalate "," (fmap (showExpr False . unbox . js) l) ++ "]"

-- Operations on arrays
newArray :: (SunroofArgument args, Sunroof a) => args -> JS t (JSArray a)
newArray args = cast `fmap` new "Array" args

emptyArray :: (Sunroof a) => JSArray a
emptyArray = box $ literal "[]"

-- | The @length@ property of arrays.
length' :: JSSelector JSNumber
length' = attr "length"

push :: (SunroofArgument a, Sunroof a) => a -> JSArray a -> JS t ()
push a = invoke "push" a

unshift :: (SunroofArgument a, Sunroof a) => a -> JSArray a -> JS t ()
unshift a = invoke "unshift" a

pop :: (Sunroof a) => JSArray a -> JS t a
pop = invoke "pop" ()

shift :: (Sunroof a) => JSArray a -> JS t a
shift = invoke "shift" ()

forEach :: (Sunroof a, SunroofArgument a) => (a -> JS A ()) -> JSArray a -> JS t ()
forEach body arr = do
        f <- function body
        arr # invoke "forEach" f :: JS t ()
        return ()