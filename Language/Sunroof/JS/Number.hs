
{-# LANGUAGE TypeFamilies #-}

-- | Numbers in Javascript.
module Language.Sunroof.JS.Number
  ( JSNumber
  , int
  ) where

import Prelude hiding (div, mod, quot, rem, floor, ceiling, isNaN, isInfinite)

import Data.Boolean ( BooleanOf, Boolean(..), IfB(..), EqB(..), OrdB(..) )
import Data.Boolean.Numbers 
  ( NumB(..)
  , RealFloatB(..), RealFracB(..)
  , IntegralB(..), fromIntegralB )
import Data.AdditiveGroup ( AdditiveGroup(..) )
import Data.VectorSpace ( VectorSpace(..) )
import Data.Ratio ( Ratio )

import Language.Sunroof.Internal ( litparen )
import Language.Sunroof.JavaScript ( Expr, showExpr, uniOp, binOp, literal )
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..) )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

-- -------------------------------------------------------------
-- JSNumber Type
-- -------------------------------------------------------------

-- | Type of numbers in Javascript.
data JSNumber = JSNumber Expr

-- | Show the Javascript
instance Show JSNumber where
  show (JSNumber v) = showExpr False v

-- | First-class values in Javascript.
instance Sunroof JSNumber where
  box = JSNumber
  unbox (JSNumber e) = e

instance Num JSNumber where
  (JSNumber e1) + (JSNumber e2) = box $ binOp "+" e1 e2
  (JSNumber e1) - (JSNumber e2) = box $ binOp "-" e1 e2
  (JSNumber e1) * (JSNumber e2) = box $ binOp "*" e1 e2
  abs (JSNumber e1) = box $ uniOp "Math.abs" e1
  signum (JSNumber _e1) = error "signum" -- JSNumber $ uniOp "ERROR" e1
  fromInteger = box . literal . litparen . show

instance NumB JSNumber where
  type IntegerOf JSNumber = JSNumber
  fromIntegerB = id

instance IntegralB JSNumber where
  quot a b = ifB ((a / b) <* 0)
                 (box $ uniOp "Math.ceil" (unbox $ a / b))
                 (a `div` b)
  rem a b = a - (a `quot` b)*b
  div a b = box $ uniOp "Math.floor" (unbox $ a / b)
  mod (JSNumber a) (JSNumber b) = box $ binOp "%" a b
  toIntegerB = id


instance Fractional JSNumber where
  (JSNumber e1) / (JSNumber e2) = box $ binOp "/" e1 e2
  fromRational = box . literal . litparen . show . (fromRational :: Rational -> Double)

instance Floating JSNumber where
  pi = box $ literal $ "Math.PI"
  sin   (JSNumber e) = box $ uniOp "Math.sin"   e
  cos   (JSNumber e) = box $ uniOp "Math.cos"   e
  asin  (JSNumber e) = box $ uniOp "Math.asin"  e
  acos  (JSNumber e) = box $ uniOp "Math.acos"  e
  atan  (JSNumber e) = box $ uniOp "Math.atan"  e
  sinh  (JSNumber e) = box $ uniOp "Math.sinh"  e
  cosh  (JSNumber e) = box $ uniOp "Math.cosh"  e
  asinh (JSNumber e) = box $ uniOp "Math.asinh" e
  acosh (JSNumber e) = box $ uniOp "Math.acosh" e
  atanh (JSNumber e) = box $ uniOp "Math.atanh" e
  exp   (JSNumber e) = box $ uniOp "Math.exp"   e
  log   (JSNumber e) = box $ uniOp "Math.log"   e

instance RealFracB JSNumber where
  properFraction n =
    ( fromIntegralB $ ifB (n >=* 0) (floor n :: JSNumber) (ceiling n :: JSNumber)
    , ifB (n >=* 0) (n - floor n) (n - ceiling n)
    )
  round   (JSNumber e) = fromIntegralB $ JSNumber $ uniOp "Math.round" e
  ceiling (JSNumber e) = fromIntegralB $ JSNumber $ uniOp "Math.ceil"  e
  floor   (JSNumber e) = fromIntegralB $ JSNumber $ uniOp "Math.floor" e

instance RealFloatB JSNumber where
  isNaN (JSNumber a) = box $ uniOp "isNaN" a
  isInfinite n = notB (isFinite n) &&* notB (isNaN n)
    where isFinite (JSNumber a) = box $ uniOp "isFinite" a
  isNegativeZero n = isInfinite n &&* n <* 0
  isIEEE _ = true -- AFAIK
  atan2 (JSNumber a) (JSNumber b) = box $ binOp "Math.atan2" a b

type instance BooleanOf JSNumber = JSBool

instance IfB JSNumber where
  ifB = jsIfB

instance EqB JSNumber where
  (==*) e1 e2 = box $ binOp "==" (unbox e1) (unbox e2)
  (/=*) e1 e2 = box $ binOp "!=" (unbox e1) (unbox e2)

instance OrdB JSNumber where
  (>*)  e1 e2 = box $ binOp ">"  (unbox e1) (unbox e2)
  (>=*) e1 e2 = box $ binOp ">=" (unbox e1) (unbox e2)
  (<*)  e1 e2 = box $ binOp "<"  (unbox e1) (unbox e2)
  (<=*) e1 e2 = box $ binOp "<=" (unbox e1) (unbox e2)

instance AdditiveGroup JSNumber where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance VectorSpace JSNumber where
  type Scalar JSNumber = JSNumber
  s *^ d = s * d

instance SunroofValue Double where
  type ValueOf Double = JSNumber
  js = box . literal . litparen . show

instance SunroofValue Float where
  type ValueOf Float = JSNumber
  js = box . literal . litparen . show

instance SunroofValue Int where
  type ValueOf Int = JSNumber
  js = fromInteger . toInteger

instance SunroofValue Integer where
  type ValueOf Integer = JSNumber
  js = fromInteger . toInteger

instance (Integral a) => SunroofValue (Ratio a) where
  type ValueOf (Ratio a) = JSNumber
  js = box . literal . litparen . (show :: Double -> String) . fromRational . toRational

-- -------------------------------------------------------------
-- JSNumber Combinators
-- -------------------------------------------------------------

-- | A explicit cast to int.
int :: (Sunroof a) => a -> JSNumber
int = box . uniOp "(int)" . unbox








