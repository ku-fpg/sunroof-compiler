
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the equivalent of a 'IORef' in the Sunroof world.
module Language.Sunroof.JS.Ref
        ( JSRef
        , newJSRef
        , readJSRef
        , writeJSRef
        , modifyJSRef
        ) where

import Data.Boolean ( BooleanOf, IfB(..), EqB(..) )

import Language.Sunroof.Classes ( Sunroof(..) )
import Language.Sunroof.Types ( T(..), JS(..), evaluate, new, (#), liftJS )
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

-- -------------------------------------------------------------
-- JSRef Type
-- -------------------------------------------------------------

-- | This is the 'IORef' of Sunroof.
newtype JSRef a = JSRef JSObject

instance (Sunroof a) => Show (JSRef a) where
  show (JSRef o) = show o

instance (Sunroof a) => Sunroof (JSRef a) where
  box = JSRef . box
  unbox (JSRef o) = unbox o

type instance BooleanOf (JSRef a) = JSBool

instance (Sunroof a) => IfB (JSRef a) where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance (Sunroof a) => EqB (JSRef a) where
  (JSRef a) ==* (JSRef b) = a ==* b

-- -------------------------------------------------------------
-- JSRef Combinators
-- -------------------------------------------------------------

-- | Create a new 'JSRef' with the given intial value.
newJSRef :: (Sunroof a) => a -> JS t (JSRef a)
newJSRef a = do
  obj <- new "Object" ()
  obj # "val" := a
  return $ JSRef obj

-- | Non-blocking read of a 'JSRef'.
readJSRef :: (Sunroof a) => JSRef a -> JS t a
readJSRef (JSRef obj) = evaluate $ obj ! "val"

-- | Non-blocking write of a 'JSRef'.
writeJSRef :: (Sunroof a) => a -> JSRef a ->  JS t ()
writeJSRef a (JSRef obj) = obj # "val" := a

-- | Non-blocking modification of a 'JSRef'.
modifyJSRef :: (Sunroof a) => (a -> JS A a) -> JSRef a -> JS t ()
modifyJSRef f ref = do
  val <- readJSRef ref
  liftJS (f val) >>= \ v -> ref # writeJSRef v
