
{-# LANGUAGE TypeFamilies #-}

-- | Booleans in Javascript.
module Language.Sunroof.JS.Bool
  ( JSBool
  , jsIfB
  ) where

import Data.Boolean ( Boolean(..), BooleanOf, IfB(..), EqB(..) )

import Language.Sunroof.JavaScript 
  ( Expr
  , operator, binOp, uniOp
  , literal, showExpr )
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..) )

-- -------------------------------------------------------------
-- JSBool Type
-- -------------------------------------------------------------

-- | Booleans in Javascript.
data JSBool = JSBool Expr

instance Show JSBool where
  show (JSBool e) = showExpr False e

instance Sunroof JSBool where
  box = JSBool
  unbox (JSBool v)  = v

instance Boolean JSBool where
  true          = box $ literal "true"
  false         = box $ literal "false"
  notB  (JSBool e1) = box $ uniOp "!" e1
  (&&*) (JSBool e1)
        (JSBool e2) = box $ binOp "&&" e1 e2
  (||*) (JSBool e1)
        (JSBool e2) = box $ binOp "||" e1 e2

type instance BooleanOf JSBool = JSBool

instance IfB JSBool where
  ifB = jsIfB

instance EqB JSBool where
  (==*) e1 e2 = box $ binOp "==" (unbox e1) (unbox e2)
  (/=*) e1 e2 = box $ binOp "!=" (unbox e1) (unbox e2)

instance SunroofValue Bool where
  type ValueOf Bool = JSBool
  js True = true
  js False = false

-- -------------------------------------------------------------
-- JSBool Combinators
-- -------------------------------------------------------------

-- | Combinator for @if-then-else@ expressions. Not intended
--   for usage. Provided as a convenience for 'Data.Boolean.IfB'
--   instances. Use 'Data.Boolean.ifB' instead.
jsIfB :: (Sunroof a) => JSBool -> a -> a -> a
jsIfB (JSBool c) t e = box $ operator "?:" [c, unbox t, unbox e]
