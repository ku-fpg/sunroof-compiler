
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'JSMap' provides an abstract and more type-safe access to maps
--   in JavaScript. It is a wrapper around the dictionary each object
--   in JavaScript is.
module Language.Sunroof.JS.Map
  ( JSMap
  , newMap
  , insert
  , lookup
  , lookup'     -- TO remove in next major version change
  , delete
  , size
  , selectors
  , elems
  ) where

import Data.Boolean ( IfB(..), BooleanOf )

import Prelude hiding ( lookup )
import Language.Sunroof.Classes
  ( Sunroof(..) )
import Language.Sunroof.Types hiding ( delete )
import qualified Language.Sunroof.Types as T
import Language.Sunroof.Selector ( JSSelector, (!) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Number
import qualified Language.Sunroof.JS.Array as A

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

{-# DEPRECATED lookup' "Don't use lookup'; use lookup instead" #-}
lookup' :: (SunroofKey k, Sunroof a) => k -> JSMap k a -> JS t a
lookup' = lookup

-- | @lookup k@ selects the value associated with the key @k@.
lookup :: (SunroofKey k, Sunroof a) => k -> JSMap k a -> JS t a
lookup k (JSMap o) = do
        evaluate $ o ! jsKey k

size :: (SunroofKey k, Sunroof a) => JSMap k a -> JS t JSNumber
size mp = do
        arr <- mp # elems
        evaluate $ arr ! A.length'

delete :: (SunroofKey k, Sunroof a) => k -> JSMap k a -> JS t ()
delete k (JSMap o) = o # T.delete (jsKey k :: JSSelector ())

-- | get the elements in a @Map@. If you want the keys, then keep
-- a second @Map@ that maps to the keys, or store the key in the target element.
elems :: forall k a t . (SunroofKey k, Sunroof a) => JSMap k a -> JS t (A.JSArray a)
elems mp = do
        a0 <- selectors mp
        a0 # jsMap (\ s -> return $ mp ! s)

selectors :: (SunroofKey k, Sunroof a) => JSMap k a -> JS t (A.JSArray (JSSelector a))
selectors (JSMap o) = fun "Object.keys" $$ o

instance SunroofKey k => SunroofFunctor (JSMap k) where
  forEach body mp = do
        arr <- mp # elems
        arr # forEach body
        return ()

  jsMap body mp = do
        mp1 <- newMap
        arr <- mp # selectors
        arr # forEach (\ k -> do
                v <- evaluate $ mp ! k
                v1 <- body v
                -- We cheat slightly, and cast the Selector
                -- therefore re-using the same structure
                -- in the cloned Map.
                mp1 # cast k := v1)
        return mp1
