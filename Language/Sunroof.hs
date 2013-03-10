
--{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}

module Language.Sunroof
  -- JavaScript
  ( Type(..)
  -- Classes
  , Sunroof(..), SunroofValue(..), JSArgument(..)
  -- Types
  , T(..), ThreadProxy(..)
  , JSThread(..), JSThreadReturn(..)
  , JS(..), JSA, JSB
  , abortJS, liftJS
  , JSFunction
  , function, continuation
  , apply, ($$)
  , cast
  , (#)
  , attribute
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , JSTuple(..)
  -- Compiler
  , staticCompiler
  , sunroofCompiler
  , CompilerOpts(..)
  -- Selector
  , JSSelector
  , label, index
  , (!)
  -- Concurrent
  , loopJS
  , forkJS
  , threadDelayJSB
  , yieldJSB
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
  , pushArray, popArray
  , shiftArray, unshiftArray
  , lookupArray
  , forEach
  -- Chan
  , JSChan
  , newChan
  , writeChan, readChan
  ) where

import Language.Sunroof.JavaScript ( Type(..) )

import Language.Sunroof.Classes 
  ( Sunroof(..), SunroofValue(..), JSArgument(..) )

import Language.Sunroof.Types
  ( T(..), ThreadProxy(..)
  , JSThread(..), JSThreadReturn(..)
  , JS(..), JSA, JSB
  , abortJS, liftJS
  , JSFunction
  , function, continuation
  , apply, ($$)
  , cast
  , (#)
  , attribute
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , JSTuple(..) )

import Language.Sunroof.Compiler
  ( staticCompiler
  , sunroofCompiler
  , CompilerOpts(..) )

import Language.Sunroof.Selector
  ( JSSelector
  , label, index
  , (!) )

import Language.Sunroof.Concurrent
  ( loopJS
  , forkJS
  , threadDelayJSB
  , yieldJSB )

import Language.Sunroof.Container
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
  , pushArray, popArray
  , shiftArray, unshiftArray
  , lookupArray
  , forEach )

import Language.Sunroof.JS.Chan
  ( JSChan
  , newChan
  , writeChan, readChan )



