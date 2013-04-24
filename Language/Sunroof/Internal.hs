
-- | Internal helper functions.
module Language.Sunroof.Internal
  ( litparen
  ) where

import Data.Char ( isDigit )

-- | Determines wether a Javascript literal, given as a string,
--   requires parenthesis and adds them if so.
litparen :: String -> String
litparen nm | all (\ c -> isDigit c || c == '.') nm = nm
            | otherwise      = "(" ++ nm ++ ")"