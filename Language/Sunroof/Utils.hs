{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Common utilities for Sunroof.
module Language.Sunroof.Utils
  ( fixJSA, fixJSB
  ) where

import Language.Sunroof.Classes
import Language.Sunroof.Types
import Language.Sunroof.JS.Ref

-- -------------------------------------------------------------
-- Fixpoint combinator
-- -------------------------------------------------------------

-- | @fixJSA@ is the fix point combinator for functions that return the JS monad.
fixJSA :: (SunroofArgument a, Sunroof b) => (JSFunction a b -> (a -> JS A b)) -> JS t (JSFunction a b)
fixJSA f = do
        ref <- newJSRef (cast nullJS)
        fn <- function $ \ a' -> do
                        fn' <- readJSRef ref
                        f fn' a'
        ref # writeJSRef fn
        return fn

-- | @fixJSB@ is the fix point combinator for continuations.
--   Be careful, this can blow the stack if there are no yields
--   or blocks in the function.
fixJSB :: (SunroofArgument a) => (JSContinuation a -> (a -> JS B ())) -> JS t (JSContinuation a)
fixJSB f = do
        ref <- newJSRef (cast nullJS)
        fn <- continuation $ \ a' -> do
                        fn' <- readJSRef ref
                        f fn' a'
        ref # writeJSRef fn
        return fn


