
--{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}

module Language.Sunroof
  -- JavaScript
  ( Type(..)
  -- Classes
  , Sunroof(..), SunroofValue(..), SunroofArgument(..)
  -- Types
  , T(..), ThreadProxy(..)
  , SunroofThread(..), SunroofThreadReturn(..)
  , JS(..), JSA, JSB
  , abort, liftJS
  , JSFunction
  , function, continuation, reify
  , apply, ($$)
  , cast
  , (#)
  , attr
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , JSTuple(..)
  -- Compiler
  , sunroofCompileJS
  , CompilerOpts(..)
  -- Selector
  , JSSelector
  , label, index
  , (!)
  -- Concurrent
  , loop
  , forkJS
  , threadDelay
  , yield
  -- Container
  , JSRef
  , newJSRef
  , readJSRef
  , writeJSRef
  , modifyJSRef
  -- Bool
  , JSBool
  -- Object
  , JSObject, this, object
  -- Number
  , JSNumber, int
  -- String
  , JSString, string
  -- Array
  , JSArray
  , array, newArray
  , lengthArray
  , forEach
  -- Chan
  , JSChan
  , newChan
  , writeChan, readChan
  -- MVar
  , JSMVar
  , newMVar
  , newEmptyMVar
  , takeMVar, putMVar
  ) where

import Language.Sunroof.JavaScript ( Type(..) )

import Language.Sunroof.Classes
  ( Sunroof(..), SunroofValue(..), SunroofArgument(..) )

import Language.Sunroof.Types
  ( T(..), ThreadProxy(..)
  , SunroofThread(..), SunroofThreadReturn(..)
  , JS(..), JSA, JSB
  , abort, liftJS
  , JSFunction
  , function, continuation, reify
  , apply, ($$)
  , cast
  , (#)
  , attr
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , JSTuple(..) )

import Language.Sunroof.Compiler
  ( sunroofCompileJS
  , CompilerOpts(..) )

import Language.Sunroof.Selector
  ( JSSelector
  , label, index
  , (!) )

import Language.Sunroof.Concurrent
  ( loop
  , forkJS
  , threadDelay
  , yield )

import Language.Sunroof.JS.Ref
  ( JSRef
  , newJSRef
  , readJSRef
  , writeJSRef
  , modifyJSRef )

import Language.Sunroof.JS.Bool ( JSBool )
import Language.Sunroof.JS.Object ( JSObject, object, this )
import Language.Sunroof.JS.Number ( JSNumber, int )
import Language.Sunroof.JS.String ( JSString, string )

import Language.Sunroof.JS.Array
  ( JSArray
  , array, newArray
  , lengthArray
  , forEach )

import Language.Sunroof.JS.Chan
  ( JSChan
  , newChan
  , writeChan, readChan )

import Language.Sunroof.JS.MVar
  ( JSMVar
  , newMVar, newEmptyMVar
  , takeMVar, putMVar )



