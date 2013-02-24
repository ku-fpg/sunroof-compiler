{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module Language.Sunroof.Active where

import Language.Sunroof.Types
import Language.Sunroof.Compiler

import Control.Newtype
import Data.Active
import Data.VectorSpace hiding ((<.>))
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Boolean

--   other numeric types.
newtype JSTime = JSTime { unJSTime :: JSNumber }
  deriving ( Show )

instance Newtype JSTime JSNumber where
  pack   = JSTime
  unpack = unJSTime

instance AffineSpace JSTime where
  type Diff JSTime = JSDuration
  (JSTime t1) .-. (JSTime t2) = JSDuration (t1 - t2)
  (JSTime t) .+^ (JSDuration d) = JSTime (t + d)

instance Clock JSTime where
  firstTime (JSTime t1) (JSTime t2) = JSTime (minB t1 t2)
  lastTime (JSTime t1) (JSTime t2) = JSTime (maxB t1 t2)
  toTime = JSTime . js . toRational
  fromTime = toFractionalOf

instance FractionalOf JSTime JSNumber where
  toFractionalOf = unJSTime

instance (BooleanOf a ~ JSBool, IfB a) => Deadline JSTime a where
        choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

newtype JSDuration = JSDuration { unJSDuration :: JSNumber }
  deriving ( Show, AdditiveGroup )

instance Newtype JSDuration JSNumber where
  pack   = JSDuration
  unpack = unJSDuration

instance VectorSpace JSDuration where
  type Scalar JSDuration = JSNumber
  s *^ (JSDuration d) = JSDuration (s * d)

instance Waiting JSDuration where
  toDuration = JSDuration . js . toRational
  fromDuration = toFractionalOf

instance FractionalOf JSDuration JSNumber where
  toFractionalOf = unJSDuration

ex1 :: Active JSTime (JS JSNumber)
ex1 = fmap return ui

reifyActiveJS :: Active JSTime (JS ()) -> JS (JSNumber, JSNumber, JSFunction JSNumber ())
reifyActiveJS = onActive (error "can not reify a constant Active (no start or end)") $ \ d -> do
        f <- function (runDynamic d . JSTime)
        return ( fromTime $ start $ era d
               , fromTime $ end $ era d
               , f
               )

{-
compileActiveJS :: Active JSTime (JS JSNumber) -> String
compileActiveJS act = a ++ " ; return " ++ b
  where (a,b) = compileJS $ do
                   obj :: JSFunction JSNumber JSNumber <- function (runActive act . JSTime)
                   apply obj 0
-}