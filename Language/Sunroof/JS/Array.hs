
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a more specific type for arrays in Javascript
--   (together with basic operations on them).
module Language.Sunroof.JS.Array
  ( JSArray
  , array, newArray
  , length'
  , lookup'
  , insert'
  , push, pop
  , shift, unshift
  , forEach
  , empty
  ) where

import Prelude hiding ( lookup, length )

import Data.List ( intercalate )
import Data.Boolean ( BooleanOf, IfB(..) )

import Language.Sunroof.JavaScript ( Expr, showExpr, literal )
import Language.Sunroof.Types
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..), SunroofArgument(..) )
import Language.Sunroof.Selector ( JSSelector, index, (!) )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Number ( JSNumber )

-- -------------------------------------------------------------
-- JSArray Type
-- -------------------------------------------------------------

-- | Type if arrays in Javascript. The type parameter
--   given the entry type.
data JSArray a = JSArray Expr

-- | Show the Javascript.
instance Show (JSArray a) where
  show (JSArray v) = showExpr False v

-- | Arrays are first-class Javascript values.
instance (Sunroof a) => Sunroof (JSArray a) where
  box = JSArray
  unbox (JSArray o) = o

-- | The boolean of arrays are 'JSBool'.
type instance BooleanOf (JSArray a) = JSBool

-- | You can write branches that return arrays.
instance (Sunroof a) => IfB (JSArray a) where
  ifB = jsIfB

-- -------------------------------------------------------------
-- JSArray Combinators
-- -------------------------------------------------------------

-- | Create a literal array from a Haskell list.
array :: (SunroofValue a, Sunroof (ValueOf a)) => [a] -> JS t (JSArray (ValueOf a))
array l  = evaluate $ box $ literal $ "[" ++ intercalate "," (fmap (showExpr False . unbox . js) l) ++ "]"

-- | Create a new array object containing the given values.
newArray :: (SunroofArgument args, Sunroof a) => args -> JS t (JSArray a)
newArray args = cast `fmap` new "Array" args

-- | The empty array.
empty :: (Sunroof a) => JS t (JSArray a)
empty = evaluate $ box $ literal "[]"

-- | The @length@ property of arrays.
length' :: JSSelector JSNumber
length' = attr "length"

-- | A type-safe version of array lookup.
lookup' :: (Sunroof a) => JSNumber -> JSArray a -> a
lookup' n e = e ! index n

-- | A type-safe version of array insert.
insert' :: (Sunroof a) => JSNumber -> a -> JSArray a -> JS t ()
insert' n a e = e # index n := a

-- | Push a element into the array as if it was a stack.
--   Returns nothing instead of the new length.
--   See <http://www.w3schools.com/jsref/jsref_push.asp>.
push :: (SunroofArgument a, Sunroof a) => a -> JSArray a -> JS t ()
push a = invoke "push" a

-- | Adds a new element to the beginning of the array (queue).
--   Returns nothing instead of the new length.
--   See <http://www.w3schools.com/jsref/jsref_unshift.asp>.
unshift :: (SunroofArgument a, Sunroof a) => a -> JSArray a -> JS t ()
unshift a = invoke "unshift" a

-- | Pop a element from the array as if it was a stack.
--   See <http://www.w3schools.com/jsref/jsref_pop.asp>.
pop :: (Sunroof a) => JSArray a -> JS t a
pop = invoke "pop" ()

-- | Removes and return the first element of an array (dequeue).
--   See <http://www.w3schools.com/jsref/jsref_shift.asp>.
shift :: (Sunroof a) => JSArray a -> JS t a
shift = invoke "shift" ()

-- | Foreach iteration method provided by most browsers.
--   Execute the given action on each element of the array.
--   See
--   <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array/forEach>,
--   <http://msdn.microsoft.com/en-us/library/ie/ff679980.aspx>.
forEach :: (Sunroof a, SunroofArgument a) => (a -> JS A ()) -> JSArray a -> JS t ()
forEach body arr = do
        f <- function body
        arr # invoke "forEach" f :: JS t ()
        return ()