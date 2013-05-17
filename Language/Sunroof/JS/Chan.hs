
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- | 'JSChan' provides the same functionality and
--   concurrency abstraction in Javascript computations
--   as 'Control.Concurrent.Chan' in Haskell.
module Language.Sunroof.JS.Chan
  ( JSChan
  , newChan
  , writeChan, readChan
  ) where

import Data.Boolean ( IfB(..), EqB(..), BooleanOf )

import Language.Sunroof.Classes
import Language.Sunroof.Types
import Language.Sunroof.Concurrent ( forkJS )
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.JS.Bool
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Array
  ( JSArray
  , newArray, length'
  , push, shift )

-- -------------------------------------------------------------
-- JSChan Type
-- -------------------------------------------------------------

-- | 'JSChan' abstraction. The type parameter gives
--   the type of values held in the channel.
newtype JSChan a = JSChan JSObject

instance (SunroofArgument o) => Show (JSChan o) where
  show (JSChan o) = show o

instance (SunroofArgument o) => Sunroof (JSChan o) where
  unbox (JSChan o) = unbox o
  box o = JSChan (box o)

instance (SunroofArgument o) => IfB (JSChan o) where
  ifB = jsIfB

type instance BooleanOf (JSChan o) = JSBool

instance (SunroofArgument o) => JSTuple (JSChan o) where
  type Internals (JSChan o) = 
    ( (JSArray (JSContinuation (JSContinuation o))) -- callbacks of written data
    , (JSArray (JSContinuation o))                 -- callbacks of waiting readers
    )
  match o = (o ! attr "written", o ! attr "waiting")
  tuple (written,waiting) = do
    o <- new "Object" ()
    o # attr "written" := written
    o # attr "waiting" := waiting
    return (JSChan o)

-- | Reference equality, not value equality.
instance (SunroofArgument o) => EqB (JSChan o) where
  (JSChan a) ==* (JSChan b) = a ==* b

-- -------------------------------------------------------------
-- JSChan Combinators
-- -------------------------------------------------------------

-- | Create a new empty 'JSChan'.
newChan :: (SunroofArgument a) => JS t (JSChan a)
newChan = do
  written <- newArray ()
  waiting <- newArray ()
  tuple (written, waiting)

-- | Put a value into the channel. This will never block.
writeChan :: forall t a . (SunroofThread t, SunroofArgument a) => a -> JSChan a -> JS t ()
writeChan a (match -> (written,waiting)) = do
  ifB ((waiting ! length') ==* 0)
      (do f <- continuation $ \ (k :: JSContinuation a) -> goto k a :: JSB ()
          _ <- written # push (f :: JSContinuation (JSContinuation a))
          return ()
      )
      (do f <- shift waiting
          forkJS (goto f a :: JSB ())
      )

-- | Take a value out of the channel. If there is no value
--   inside, this will block until one is available.
readChan :: forall a . (SunroofArgument a) => JSChan a -> JS B a
readChan (match -> (written,waiting)) = do
  ifB ((written ! length') ==* 0)
      (do -- Add yourself to the 'waiting for writer' Q.
          callcc $ \ k -> do _ <- waiting # push (k :: JSContinuation a)
                             done
      )
      (do f <- shift written
          -- Here, we add our continuation into the written Q.
          callcc $ \ k -> goto f k
      )



