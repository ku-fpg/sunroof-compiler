
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Sunroof.Active
  ( JSTime
  , JSDuration
  , reifyActive
  ) where

import Control.Newtype ( Newtype(..) )

import Data.Active 
  ( Clock(..), Deadline(..), Waiting(..)
  , Active
  , FractionalOf
  , toFractionalOf, onActive, runDynamic
  , start, era, end )
import Data.VectorSpace ( VectorSpace(..) )
import Data.AffineSpace ( AffineSpace(..) )
import Data.AdditiveGroup ( AdditiveGroup(..) )
import Data.Boolean 
  ( BooleanOf, IfB(..), OrdB(..)
  , minB, maxB )

import Language.Sunroof.Types 
  ( T(..)
  , JS, SunroofThread
  , JSFunction
  , function )
import Language.Sunroof.Classes ( Sunroof, SunroofValue(..), SunroofArgument(..) )
import Language.Sunroof.JS.Bool ( JSBool )
import Language.Sunroof.JS.Number ( JSNumber )

-- -------------------------------------------------------------
-- JSTime Type
-- -------------------------------------------------------------

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

instance (Sunroof a, SunroofArgument a, SunroofThread t) => Deadline JSTime (JS t a) where
  choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

instance Deadline JSTime JSNumber where
  choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

instance Deadline JSTime JSBool where
  choose (JSTime t1) (JSTime t2) = ifB (t1 <=* t2)

--instance (BooleanOf a ~ JSBool, IfB a) => Deadline JSTime (f -> a) where
--        choose (JSTime t1) (JSTime t2) x y c = ifB (t1 <=* t2) (x c) (y c)

-- -------------------------------------------------------------
-- JSDuration Type
-- -------------------------------------------------------------

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

-- -------------------------------------------------------------
-- Active Combinators
-- -------------------------------------------------------------

--ex1 :: Active JSTime (JS t JSNumber)
--ex1 = fmap return ui

reifyActive :: Active JSTime (JS A ()) 
              -> JS A (JSNumber, JSNumber, JSFunction JSNumber ())
reifyActive = onActive
  (\ x -> do 
    f <- function (\ _ -> x)
    return ( 0, 0, f )
  )
  (\ d -> do 
    f <- function (runDynamic d . JSTime)
    return ( fromTime $ start $ era d
           , fromTime $ end $ era d
           , f )
  )

{-
compileActiveJS t :: Active JSTime (JS t JSNumber) -> String
compileActiveJS t act = a ++ " ; return " ++ b
  where (a,b) = compileJS t $ do
                   obj :: JSFunction JSNumber JSNumber <- function (runActive act . JSTime)
                   apply obj 0
-}