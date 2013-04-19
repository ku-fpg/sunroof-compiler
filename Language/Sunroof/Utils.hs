{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Common utilities for Sunroof.
module Language.Sunroof.Utils
  ( comment, fixJS
  , substr, substr2
  ) where

import Language.Sunroof.Classes
import Language.Sunroof.Types
import Language.Sunroof.JS.String
import Language.Sunroof.JS.Number

-- -------------------------------------------------------------
-- Comments
-- -------------------------------------------------------------

-- | Write a JavaScript comment into the generated source.
comment :: String -> JS t ()
comment = single . JS_Comment

-- -------------------------------------------------------------
-- Fixpoint combinator
-- -------------------------------------------------------------

-- | @jsfix@ is the @mfix@ for the JS Monad.
fixJS :: (SunroofArgument a) => (a -> JSA a) -> JS t a
fixJS = single . JS_Fix

-- -------------------------------------------------------------
-- JSString Combinators
-- -------------------------------------------------------------

-- These should go into JS.String, but that would cause a module cycle.

substr :: JSNumber -> JSString -> JS t JSString
substr n s = s #  invoke "substr" n

substr2 :: JSNumber -> JSNumber -> JSString -> JS t JSString
substr2 n m s = s #  invoke "substr" (n,m)

