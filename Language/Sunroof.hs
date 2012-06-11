{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}
module Language.Sunroof where

import Control.Monad.Operational

import Language.Sunroof.Compiler
import Language.Sunroof.Types

import Web.KansasComet (Template(..), extract)

-- The API of Javascript, reflected

alert :: JSString -> JS ()
alert msg = call "alert" <$> with [cast msg]

