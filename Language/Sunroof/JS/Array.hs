
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Sunroof.JS.Array
  ( JSArray
  , array, newArray
  , lengthArray
  , pushArray, popArray
  , shiftArray, unshiftArray
  , lookupArray
  , forEach
  ) where

import Data.List ( intercalate )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Boolean ( BooleanOf, IfB(..) )

import Language.Sunroof.JavaScript ( Expr, showExpr, literal, binOp )
import Language.Sunroof.Types 
  ( JS, JSArgument(..), T(A)
  , cast, invoke, function, evaluate, object, attribute
  , (#) )
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..) )
import Language.Sunroof.Selector ( index, (!) )
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
array l  = box $ literal $ "[" ++ intercalate "," (fmap (showVar . js) l) ++ "]"

-- Operations on arrays
newArray :: (Sunroof a) => JS t (JSArray a)
newArray = evaluate $ cast $ object "new Array()"

emptyArray :: (Sunroof a) => JSArray a
emptyArray = box $ literal "[]"

lengthArray :: (Sunroof a) => JSArray a -> JSNumber
lengthArray o = cast o ! attribute "length"

pushArray :: (JSArgument a, Sunroof a) => a -> JSArray a -> JS t ()
pushArray a = invoke "push" a

unshiftArray :: (JSArgument a, Sunroof a) => a -> JSArray a -> JS t ()
unshiftArray a = invoke "unshift" a

popArray :: (Sunroof a) => JSArray a -> JS t a
popArray = invoke "pop" ()

shiftArray :: (Sunroof a) => JSArray a -> JS t a
shiftArray = invoke "shift" ()

lookupArray :: (Sunroof a) => JSNumber -> JSArray a -> a
lookupArray idx arr = cast arr ! index idx

forEach :: (Sunroof a, JSArgument a) => (a -> JS A ()) -> JSArray a -> JS t ()
forEach body arr = do
        f <- function body
        arr # invoke "forEach" f :: JS t ()
        return ()