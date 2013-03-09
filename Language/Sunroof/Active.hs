{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, MultiParamTypeClasses, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module Language.Sunroof.Active where

import Language.Sunroof.Types
import Language.Sunroof.Classes ( Sunroof, SunroofValue(..) )
--import Language.Sunroof.Compiler

import Control.Newtype
import Data.Active
import Data.VectorSpace hiding ((<.>))
--import Data.AdditiveGroup
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

--instance (BooleanOf a ~ JSBool, IfB a) => Deadline JSTime a where
--        choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

instance (BooleanOf b ~ JSBool, IfB b, Deadline JSTime b) => Deadline JSTime (a -> b) where
        choose (JSTime t1) (JSTime t2) f1 f2 a = ifB (t1 <=* t2) (f1 a) (f2 a)

instance (Sunroof a, JSArgument a, JSThread t) => Deadline JSTime (JS t a) where
        choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

instance Deadline JSTime JSNumber where
        choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

instance Deadline JSTime JSBool where
        choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

--instance (BooleanOf a ~ JSBool, IfB a) => Deadline JSTime (f -> a) where
--        choose (JSTime t1) (JSTime t2) x y c = ifB (t1 <=* t2) (x c) (y c)


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

ex1 :: Active JSTime (JS t JSNumber)
ex1 = fmap return ui

reifyActiveJS :: Active JSTime (JS A ()) -> JS A (JSNumber, JSNumber, JSFunction JSNumber ())
reifyActiveJS = onActive
              (\ x -> do f <- function (\ _ -> x)
                         return ( 0
                                , 0
                                , f
                                )
              )
              (\ d -> do f <- function (runDynamic d . JSTime)
                         return ( fromTime $ start $ era d
                                , fromTime $ end $ era d
                                , f
                                )
              )

{-
compileActiveJS t :: Active JSTime (JS t JSNumber) -> String
compileActiveJS t act = a ++ " ; return " ++ b
  where (a,b) = compileJS t $ do
                   obj :: JSFunction JSNumber JSNumber <- function (runActive act . JSTime)
                   apply obj 0
-}