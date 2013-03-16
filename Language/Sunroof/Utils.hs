{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Sunroof.Utils
  ( fixJS
  ) where

import Language.Sunroof.Classes
import Language.Sunroof.Types
import Language.Sunroof.JS.Ref

-- -------------------------------------------------------------
-- Fixpoint combinator
-- -------------------------------------------------------------

--  | @fixJS@ is the fix point combinator for functions that return the JS monad.
fixJS :: (SunroofArgument a, Sunroof b, t ~ A) => ((a -> JS t b) -> (a -> JS t b)) -> a -> JS t b
fixJS f a = do
        ref <- newJSRef (cast nullJS)
        fn <- function $ \ a' -> do
                        fn' <- readJSRef ref
                        f (apply fn') a'
        ref # writeJSRef fn
        apply fn a



