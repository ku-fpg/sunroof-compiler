{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Language.Sunroof where

import Control.Monad.Operational

import Language.Sunroof.Compiler
import Language.Sunroof.Types

import Web.KansasComet (Template(..), extract)

infixl 4 <*>
infixl 4 <$>

(<*>) :: (Sunroof a) => JSM JSObject -> JSS a -> JSM a
(<*>) m s = m >>= \ o -> o <$> s

(<$>) :: (Sunroof a) => JSObject -> JSS a -> JSM a
(<$>) o s = singleton $ o `JS_Dot` s

(!) :: forall a . (Sunroof a) => JSObject -> JSString -> a
(!) arr idx = cast $ JSValue $ Op "[]" [unbox arr,unbox idx]

true = JSBool (Lit "true")
false = JSBool (Lit "false")

field :: String -> JSF a
field = JSF_Field

loop :: JSM () -> JSM ()
loop = singleton . JS_Loop

alert :: JSString -> JSM ()
alert msg = jsSelect $ JSS_Call "alert" [cast msg] :: JSM ()

jsSelect :: (Sunroof a) => JSS a -> JSM a
jsSelect = singleton . JS_Select

wait :: Template event -> JSM JSObject
wait = singleton . JS_Wait

{-
send :: Document -> JSM a -> IO a
send doc jsm = do
        print $ compile jsm
-}
-----------------------------------------------------------

test2 :: JSM ()
test2 = jsSelect $ JSS_Call "foo" [cast (1 :: JSNumber)]

run_test2 = compileJS test2

test3 :: JSM ()
test3 = do
        jsSelect $ JSS_Call "foo1" [cast (1 :: JSNumber)] :: JSM ()
        (n :: JSNumber) <- jsSelect $ JSS_Call "foo2" [cast (2 :: JSNumber)]
        jsSelect $ JSS_Call "foo3" [cast (3 :: JSNumber), cast n] :: JSM ()

run_test3 = compileJS test3

-- This works in the browser
test4 :: JSM ()
test4 = do
        alert("A")
        alert("B")

run_test4 = compileJS test4

foo :: JSNumber -> JSS ()
foo msg = JSS_Call "foo" [cast msg] :: JSS ()

test5 :: JSM ()
test5 = do
        let c = mkVar 0 :: JSObject
        c <$> foo (1)
        return ()

run_test5 = compileJS test5
