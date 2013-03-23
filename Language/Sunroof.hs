
--{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}

module Language.Sunroof
  (
  -- * Sunroof Compiler
    sunroofCompileJS
  , CompilerOpts(..)
  -- * Classes
  , Sunroof(..), SunroofValue(..), SunroofArgument(..)
  , JSTuple(..)
  -- * Types
  , Type(..)
  , T(..), ThreadProxy(..)
  , SunroofThread(..)
  , JS(..), JSA, JSB
  , JSFunction
  , JSContinuation
  , JSSelector
    -- * DSL Primitives and Utilties
  , done, liftJS
  , function, continuation
  , apply, ($$)
  , cast
  , (#)
  , attr
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , label, index
  , (!)
  , callcc
  , comment
  -- * Concurrency Primitives
  , forkJS
  , threadDelay
  , yield
  -- * Basic JS types
  -- ** JavaScript Object
  , JSObject, this, object
  -- ** Boolean
  , JSBool
  -- ** Numbers
  , JSNumber, int
  -- ** Strings
  , JSString, string
  -- ** Array
  , JSArray
  , array, newArray
  , length'
  , shift, unshift
  , pop, push
  , forEach
  , empty
  -- ** References
  , JSRef
  , newJSRef
  , readJSRef
  , writeJSRef
  , modifyJSRef
  -- ** Channnels
  , JSChan
  , newChan
  , writeChan, readChan
  -- ** Thread-safe Mutable Variables
  , JSMVar
  , newMVar
  , newEmptyMVar
  , takeMVar, putMVar
    -- * DSL Utilties
  , loop
  , fixJSA, fixJSB
  , jsfix
  ) where

import Language.Sunroof.JavaScript ( Type(..) )

import Language.Sunroof.Classes
  ( Sunroof(..), SunroofValue(..), SunroofArgument(..) )

import Language.Sunroof.Types
  ( T(..), ThreadProxy(..)
  , SunroofThread(..)
  , JS(..), JSA, JSB
  , done, liftJS
  , JSFunction
  , JSContinuation
  , function, continuation, callcc
  , apply, ($$)
  , cast
  , (#)
  , attr
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , JSTuple(..)
  )

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
  , length'
  , forEach
  , shift, unshift
  , pop, push
  , empty )

import Language.Sunroof.JS.Chan
  ( JSChan
  , newChan
  , writeChan, readChan )

import Language.Sunroof.JS.MVar
  ( JSMVar
  , newMVar, newEmptyMVar
  , takeMVar, putMVar )

import Language.Sunroof.Utils
  ( comment, jsfix, fixJSA, fixJSB )
