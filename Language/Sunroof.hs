{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}
module Language.Sunroof where

import Control.Monad.Operational

import Language.Sunroof.Compiler
import Language.Sunroof.Types

import Web.KansasComet (Template(..), extract)

--infixl 4 <*>
infixl 4 <$>

--(<*>) :: (Sunroof a) => JS JSObject -> JSS a -> JS a
--(<*>) m s = m >>= \ o -> o <$> s

(<$>) :: (Sunroof a, Sunroof b) => a -> Action a b -> JS b
(<$>) o s = singleton $ o `JS_App` s

--(<.>) :: (a -> b) -> Action (b -> c) -> Action (a -> c)
--(<.>) f (
{-
examples:

        obj ! field ! function-name <$> apply (args)
        obj ! field <$> field := arg

        obj <$> foo (...)
-}
--(<.>) :: (Sunroof a, Sunroof b) => Action b -> JS b
--(<.>) o s = singleton $ o `JS_App` s

-- This is more direct that the others.
-- It can not modify the value, but can look it up.
-- Hum, not sure about this.
--(!) :: forall a . (Sunroof a) => JSObject -> JSSelector a -> a
--(!) arr (JSSelector idx) = cast $ JSValue $ Op "[]" [unbox arr,unbox idx]

true = JSBool (Lit "true")
false = JSBool (Lit "false")

{-
field :: String -> JSF a
field = JSF_Field
-}

loop :: JS () -> JS ()
loop = singleton . JS_Loop

alert :: JSString -> JS ()
alert msg = function "alert" <$> with [cast msg]

--jsSelect :: (Sunroof a) => JSS a -> JS a
--jsSelect = singleton . JS_Select

wait :: Template event -> JS JSObject
wait = singleton . JS_Wait

-- Need better utility functions here

{-
send :: Document -> JS a -> IO a
send doc jsm = do
        print $ compile jsm
-}
-----------------------------------------------------------
{-
test2 :: JS ()
test2 = jsSelect $ JSS_Call "foo" [cast (1 :: JSNumber)]

run_test2 = compileJS test2

test3 :: JS ()
test3 = do
        jsSelect $ JSS_Call "foo1" [cast (1 :: JSNumber)] :: JS ()
        (n :: JSNumber) <- jsSelect $ JSS_Call "foo2" [cast (2 :: JSNumber)]
        jsSelect $ JSS_Call "foo3" [cast (3 :: JSNumber), cast n] :: JS ()

run_test3 = compileJS test3

-- This works in the browser
test4 :: JS ()
test4 = do
        alert("A")
        alert("B")

run_test4 = compileJS test4

foo :: JSNumber -> JSS ()
foo msg = JSS_Call "foo" [cast msg] :: JSS ()

{-
test5 :: JS ()
test5 = do
        let c = mkVar 0 :: JSObject
        c <$> foo (1)
        return ()

run_test5 = compileJS test5
-}
-}

--object :: String -> JSObject
--object =
{-
method :: JSSelector (JSFunction a) -> [JSValue] -> Action JSObject a
method str args = (! str) `Map` with args

object :: String -> JSObject
object = JSObject . Lit

function :: String -> JSFunction a
function = JSFunction . Lit

with :: [JSValue] -> Action (JSFunction a) a
with = Invoke
-}