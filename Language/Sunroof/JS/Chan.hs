
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

import Data.Boolean ( IfB(..), EqB(..) )

import Language.Sunroof.Classes
  ( Sunroof(..), SunroofArgument(..) )
import Language.Sunroof.Types
import Language.Sunroof.Concurrent ( forkJS )
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.TH
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

deriveJSTuple
  [d| instance (SunroofArgument o) => JSTuple (JSChan o) where
          type Internals (JSChan o) =
                  ( (JSArray (JSContinuation (JSContinuation o))) -- callbacks of written data
                  , (JSArray (JSContinuation o))                 -- callbacks of waiting readers
                  )
  |]

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
          written # push (f :: JSContinuation (JSContinuation a))
      )
      (do f <- shift waiting
          forkJS (goto f a :: JSB ())
      )

-- | Take a value out of the channel. If there is no value
--   inside, this will block until one is available.
readChan :: forall a . (Sunroof a, SunroofArgument a) => JSChan a -> JS B a
readChan (match -> (written,waiting)) = do
  ifB ((written ! length') ==* 0)
      (do -- Add yourself to the 'waiting for writer' Q.
          callcc $ \ k -> do waiting # push (k :: JSContinuation a)
                             done
      )
      (do f <- shift written
          -- Here, we add our continuation into the written Q.
          callcc $ \ k -> goto f k
      )



