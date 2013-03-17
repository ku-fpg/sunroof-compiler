{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Sunroof.Utils
  ( fixJSA, fixJSB
  ) where

import Language.Sunroof.Classes
import Language.Sunroof.Types
import Language.Sunroof.JS.Ref

-- -------------------------------------------------------------
-- Fixpoint combinator
-- -------------------------------------------------------------

--  | @fixJS@ is the fix point combinator for functions that return the JS monad.
fixJSA :: (SunroofArgument a, Sunroof b) => (JSFunction a b -> (a -> JS A b)) -> JS t (JSFunction a b)
fixJSA f = do
        ref <- newJSRef (cast nullJS)
        fn <- function $ \ a' -> do
                        fn' <- readJSRef ref
                        f fn' a'
        ref # writeJSRef fn
        return fn

--  | @fixJS@ is the fix point combinator for continuations.
fixJSB :: (SunroofArgument a) => (JSContinuation a -> (a -> JS B ())) -> JS t (JSContinuation a)
fixJSB f = do
        ref <- newJSRef (cast nullJS)
        fn <- continuation $ \ a' -> do
                        fn' <- readJSRef ref
                        f fn' a'
        ref # writeJSRef fn
        return fn


