
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
  , insertMap
  , lookupMap
  ) where

import Data.Boolean ( IfB(..), EqB(..), BooleanOf )

import Language.Sunroof.Classes
  ( Sunroof(..), SunroofArgument(..) )
import Language.Sunroof.Types
import Language.Sunroof.Concurrent ( forkJS )
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.TH
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Array

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

insertMap :: (SunroofKey k, Sunroof a) => k -> a -> JSMap k a -> JS t ()
insertMap k a (JSMap o) = do
        o # jsKey k := a

lookupMap :: (SunroofKey k, Sunroof a) => k -> JSMap k a -> JS t a
lookupMap k (JSMap o) = do
        evaluate $ o ! jsKey k
