
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Strings in Javascript.
module Language.Sunroof.JS.String
  ( JSString
  , string
  ) where

import Data.Boolean ( BooleanOf, IfB(..), EqB(..) )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Char ( isAscii, isControl, ord )
import Data.String ( IsString(..) )

import Numeric ( showHex )

import Language.Sunroof.JavaScript ( Expr, showExpr, binOp, literal )
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..) )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

-- -------------------------------------------------------------
-- JSString Type
-- -------------------------------------------------------------

-- | Javascript string type.
data JSString = JSString Expr

-- | Show the Javascript.
instance Show JSString where
  show (JSString v) = showExpr False v

-- | First-class Javascript value.
instance Sunroof JSString where
  box = JSString
  unbox (JSString e) = e

-- | Semigroup under concatination.
instance Semigroup JSString where
  (JSString e1) <> (JSString e2) = box $ binOp "+" e1 e2

-- | Monoid under concatination and empty string.
instance Monoid JSString where
  mempty = fromString ""
  mappend (JSString e1) (JSString e2) = box $ binOp "+" e1 e2

-- | Create them from Haskell 'String's.
instance IsString JSString where
  fromString = box . literal . jsLiteralString

type instance BooleanOf JSString = JSBool

instance IfB JSString where
  ifB = jsIfB

-- | Value equality.
instance EqB JSString where
  (==*) e1 e2 = box $ binOp "==" (unbox e1) (unbox e2)
  (/=*) e1 e2 = box $ binOp "!=" (unbox e1) (unbox e2)

-- | Create a 'JSString' from a 'String'.
instance SunroofValue [Char] where
  type ValueOf [Char] = JSString
  js = fromString

-- | Create a single character 'JSString' from a 'Char'.
instance SunroofValue Char where
  type ValueOf Char = JSString
  js c = fromString [c]

-- -------------------------------------------------------------
-- JSString Combinators
-- -------------------------------------------------------------

-- | Create a Javascript string from a Haskell string.
string :: String -> JSString
string = fromString

-- -------------------------------------------------------------
-- String Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Transform a Haskell string into a string representing a JS string literal.
jsLiteralString :: String -> String
jsLiteralString = jsQuoteString . jsEscapeString

-- | Add quotes to a string.
jsQuoteString :: String -> String
jsQuoteString s = "\"" ++ s ++ "\""

-- | Transform a character to a string that represents its JS
--   unicode escape sequence.
jsUnicodeChar :: Char -> String
jsUnicodeChar c =
  let hex = showHex (ord c) ""
  in ('\\':'u': replicate (4 - length hex) '0') ++ hex

-- | Correctly replace Haskell characters by the JS escape sequences.
jsEscapeString :: String -> String
jsEscapeString [] = []
jsEscapeString (c:cs) = case c of
  -- Backslash has to remain backslash in JS.
  '\\' -> '\\' : '\\' : jsEscapeString cs
  -- Special control sequences.
  '\0' -> jsUnicodeChar '\0' ++ jsEscapeString cs -- Ambigous with numbers
  '\a' -> jsUnicodeChar '\a' ++ jsEscapeString cs -- Non JS
  '\b' -> '\\' : 'b' : jsEscapeString cs
  '\f' -> '\\' : 'f' : jsEscapeString cs
  '\n' -> '\\' : 'n' : jsEscapeString cs
  '\r' -> '\\' : 'r' : jsEscapeString cs
  '\t' -> '\\' : 't' : jsEscapeString cs
  '\v' -> '\\' : 'v' : jsEscapeString cs
  '\"' -> '\\' : '\"' : jsEscapeString cs
  '\'' -> '\\' : '\'' : jsEscapeString cs
  -- Non-control ASCII characters can remain as they are.
  c' | not (isControl c') && isAscii c' -> c' : jsEscapeString cs
  -- All other non ASCII signs are escaped to unicode.
  c' -> jsUnicodeChar c' ++ jsEscapeString cs 
