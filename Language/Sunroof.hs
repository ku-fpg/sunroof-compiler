
-- | Sunroof provides a way to express Javascript computations in
--   Haskell. The computations can be expressed using the 'JS' monad.
--
--   There are ready to use API bindings for frequently used
--   Javascript:
--
--    * 'Language.Sunroof.JS.Browser' - Bindings of the standard browser APIs.
--
--    * 'Language.Sunroof.JS.Canvas' - Bindings of the HTML5 canvas element API.
--
--    * 'Language.Sunroof.JS.JQuery' - Bindings of some JQuery methods.
--
--    * 'Language.Sunroof.JS.Date' - Bindings of the standard data API.
--
--   It also provides an abstraction over Javascripts (not existing) threading
--   model. Cooperative multithreading can be emulated using the Sunroof
--   abstractions ('forkJS', 'yield', 'loop'). Equivalents of well-known
--   Haskell concurrency abstractions like 'Control.Concurrent.MVar'
--   or 'Control.Concurrent.Chan' are also provided on Javascript level
--   through 'JSMVar' and 'JSChan'.
--
--   Due to the threading abstraction there are two kinds of computations.
--   They are indicated by the first type parameter of 'JS' (a 'T' value).
--   Normal Javascript computations that can be assumed to terminate and
--   that may deliver a result value are written in the 'JSA' monad. While
--   possibly blocking computations (those that involve threading operations)
--   are written in the 'JSB' monad.
--
--   As the computations are expressed in Haskell, they have a functional
--   nature. It is possible to change the attribute values of objects using
--   ':=' and 'Language.Sunroof.Types.#':
--
-- > o # att := val
--
--   If a top-level mutable variable is needed, use the 'JSRef' abstraction.
--   It is comparable to 'Data.IORef.IORef'.
module Language.Sunroof
  (
  -- * Notes
  -- | It is advised to use Sunroof with the following language extensions:
  --
  --   * @OverloadedStrings@ - Enables using literal strings for attribute
  --     names and Javascript strings.
  --
  --   * @DataKinds@ - Enables using @JS A@ or @JS B@ instead of @JSA@ and @JSB@.
  --     This extension is not essential.
  --

  -- * Sunroof Compiler
    sunroofCompileJSA
  , sunroofCompileJSB
  , CompilerOpts(..)
  -- * Classes
  , Sunroof(..), SunroofValue(..), SunroofArgument(..)
  , JSTuple(..)
  , SunroofKey(..)
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
  , apply, ($$), goto
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
  , delete
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
  , lookup'
  , insert'
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
  , fixJS
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
  , function, continuation
  , callcc
  , apply, ($$), goto
  , cast
  , (#)
  , attr
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , delete
  , JSTuple(..)
  , SunroofKey(..)
  )

import Language.Sunroof.Compiler
  ( sunroofCompileJSA
  , sunroofCompileJSB
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
  , lookup'
  , insert'
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
  ( comment, fixJS )
