
-- | Internal helper functions.
module Language.Sunroof.Internal
  ( proxyOf
  , litparen
  ) where

import Data.Char ( isDigit )
import Data.Proxy ( Proxy(Proxy) )

-- | Helps to get the proxy of a value.
proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | Determines wether a Javascript literal, given as a string,
--   requires parenthesis and adds them if so.
litparen :: String -> String
litparen nm | all (\ c -> isDigit c || c == '.') nm = nm
            | otherwise      = "(" ++ nm ++ ")"