{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Language.Sunroof.Painting where

import Language.Sunroof.Types
import qualified Language.Sunroof.JS.Canvas as C

import Data.Monoid
import Data.Semigroup
import Data.Boolean

-- -----------------------------------------------------------------------

newtype Painting = Painting (JSObject -> JS ())

instance Monoid Painting where
        mempty = Painting $ const $ return ()
        mappend (Painting p1) (Painting p2) = Painting $ \ cxt -> do
                        p1 cxt
                        p2 cxt

instance Semigroup Painting where
        (<>) = mappend

type instance BooleanOf Painting = JSBool

instance IfB Painting where
        ifB b (Painting p1) (Painting p2) = Painting $ \ cxt -> ifB b (p1 cxt) (p2 cxt)


-- | draw a painting onto a (canvas) object.
draw :: JSObject -> Painting -> JS ()
draw obj (Painting fn) = fn obj

-- -----------------------------------------------------------------------

-- | Turn an action on a canvas into a painting.
-- Often, this action will be a compound action.
painting :: Action JSObject () -> Painting
painting act = Painting $ \ o -> o # act

rotateP :: JSNumber -> Painting -> Painting
rotateP by (Painting m) = painting $ \ cxt -> do
        cxt # C.save
        cxt # C.rotate by
        m cxt
        cxt # C.restore
