
{-# LANGUAGE TypeFamilies #-}

-- | 'JSMap' provides an abstract and more type-safe access to maps
--   in JavaScript. It is a wrapper around the dictionary each object
--   in JavaScript is.
module Language.Sunroof.JS.Map
  ( JSMap
  , newMap
  , insert
  , lookup'
  , size
  ) where

import Data.Boolean ( IfB(..), BooleanOf )

import Language.Sunroof.Classes
  ( Sunroof(..) )
import Language.Sunroof.Types
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Number

-- -------------------------------------------------------------
-- JSMap Type
-- -------------------------------------------------------------

-- | 'JSMap' abstraction. The first type parameter gives
--   the type of keys used by the name and the second gives
--   the type of values.
newtype JSMap k a = JSMap JSObject

instance (SunroofKey k, Sunroof a) => Show (JSMap k a) where
  show (JSMap o) = show o

instance (SunroofKey k, Sunroof a) => Sunroof (JSMap k a) where
  box = JSMap . box
  unbox (JSMap o) = unbox o

type instance BooleanOf (JSMap k a) = JSBool

instance (SunroofKey k, Sunroof a) => IfB (JSMap k a) where
  ifB = jsIfB

-- -------------------------------------------------------------
-- JSMap Combinators
-- -------------------------------------------------------------

-- | Create a new empty 'JSMap'.
newMap :: JS t (JSMap k a)
newMap = do
  o <- new "Object" ()
  return $ JSMap o

-- | @insert k x@ inserts an element @x@ associated with the given
--   key @k@ into a map.
insert :: (SunroofKey k, Sunroof a) => k -> a -> JSMap k a -> JS t ()
insert k a (JSMap o) = do
        o # jsKey k := a

-- | @lookup k@ selects the value associated with the key @k@.
lookup' :: (SunroofKey k, Sunroof a) => k -> JSMap k a -> JS t a
lookup' k (JSMap o) = do
        evaluate $ o ! jsKey k

size :: JSMap k a -> JS t JSNumber
size (JSMap o) = fun "Object.size" $$ o

--deleteMap :: (SunroofKey k, Sunroof a) => k -> JSMap k a -> JS t ()
--deleteMap k (JSMap o) = fun "delete" $$ (o ! jsKey k)


