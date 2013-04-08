
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- | 'JSMVar' provides the same functionality and
--   concurrency abstraction in Javascript computations
--   as 'Control.Concurrent.MVar' in Haskell.
module Language.Sunroof.JS.Map
  ( JSMap
  , newMap
  , insert
  , lookup
  ) where

import Data.Boolean ( IfB(..), BooleanOf )

import Language.Sunroof.Classes
  ( Sunroof(..) )
import Language.Sunroof.Types
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

-- -------------------------------------------------------------
-- JSMap Type
-- -------------------------------------------------------------

-- | 'JSMap' abstraction. The type parameter gives
--   the type of values held in a 'JSMVar'.
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
-- JSMVar Combinators
-- -------------------------------------------------------------

-- | Create a new 'JSMVar' with the given value inside.
--   See 'newEmptyMVar'.
newMap :: JS t (JSMap k a)
newMap = do
  o <- new "Object" ()
  return $ JSMap o

insert :: (SunroofKey k, Sunroof a) => k -> a -> JSMap k a -> JS t ()
insert k a (JSMap o) = do
        o # jsKey k := a

lookup :: (SunroofKey k, Sunroof a) => k -> JSMap k a -> JS t a
lookup k (JSMap o) = do
        evaluate $ o ! jsKey k
