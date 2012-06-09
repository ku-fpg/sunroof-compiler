{-# LANGUAGE GADTs, RankNTypes #-}
module Language.Sunroof.Compiler where

import qualified Control.Applicative as App
import Control.Monad.Operational
import Control.Monad.State
import Data.List (intercalate)

import Language.Sunroof.Types
import Web.KansasComet (Template(..), extract)


infix  5 :=

compileJS :: (Sunroof a) => JSM a -> String
compileJS = flip evalState 0 . compile

-- define primitive effects / "instructions" for the JSM monad
data JSMI a where
    -- selector (function, property, or property assignment)
    JS_Select :: (Sunroof a) => JSS a -> JSMI a

    -- object . <selector>
    JS_Dot    :: (Sunroof a) => JSObject -> JSS a -> JSMI a

    JS_Wait   :: Template a -> JSMI JSObject
{-
    -- You can build functions, and pass them round
    JS_Function :: (Sunroof a, Sunroof b)
                => (a -> JSM b)
                -> JSM (JSFunction a b)
    -- You can invoke functions
    JS_Invoke :: JSFunction a b -> a -> JSM b
-}
    JS_Loop :: JSM () -> JSMI ()

-- Control.Monad.Operational makes a monad out of JSM for us
type JSM a = Program JSMI a

-- compile an existing expression
compile :: Sunroof c => JSM c -> CompM String
compile = eval . view
    -- since the type  Program  is abstract (for efficiency),
    -- we have to apply the  view  function first,
    -- to get something we can pattern match on
    where eval :: Sunroof b => ProgramView JSMI b -> CompM String
          -- either we call a primitive JavaScript function
          eval (JS_Select jss :>>= g) = do
            txt1 <- compileJSS jss
            compileBind txt1 g
          eval (JS_Dot o jss :>>= g) = do
            sel_txt <- compileJSS jss
            compileBind ("(" ++ showVar o ++ ")." ++ sel_txt) g
          eval (JS_Loop body :>>= g) = do -- note, we do nothing with g, as it's unreachable
            -- create a new name for our loop
            loop <- newLoop
            -- the magic: add call to loop at end of loop body instructions,
            -- this way, if body contains a JS_Wait, it gets sucked into the
            -- event callback!
            loop_body <- compile (body >>= (\() -> singleton (JS_Select (JSS_Call loop []) :: JSMI ())))
            -- define loop function, and call it once
            return $ "var " ++ loop ++ " = function(){ " ++ loop_body ++ " }; " ++ loop ++ "();"
          eval (JS_Wait tmpl :>>= g) = do
            a <- newVar
            txt2 <- compile (g a)
            let eventNames = map fst $ extract tmpl
            return $ "$.kc.waitFor(" ++ show eventNames ++ ",function(" ++ showVar a ++ "){" ++ txt2 ++ "})"

          -- or we're done already
          eval (Return b) = return $ showVar b

compileBind :: (Sunroof a, Sunroof b) => String -> (a -> JSM b) -> CompM String
compileBind txt1 m2 = do
    a <- newVar
    txt2 <- compile (m2 a)
    return $ assignVar a ++ txt1 ++ ";" ++ txt2

data JSS a where
    JSS_Call   :: String -> [JSValue]       -> JSS a
    JSS_Select :: String                    -> JSS a
    (:=)       :: (Sunroof a) => JSF a -> a -> JSS ()

data JSF a where
    JSF_Field  :: String -> JSF a

compileJSS :: (Sunroof a) => JSS a -> CompM String
compileJSS (JSS_Call nm args) = do
        -- This show is doing the compile
        return $ nm ++ "(" ++ intercalate "," (map show args) ++ ")"
compileJSS (JSS_Select nm) = return nm
compileJSS ((JSF_Field nm) := arg) = do
        return $ nm ++ " = (" ++ show arg ++ ")"

type CompM a = State Uniq a

uniqM :: CompM Uniq
uniqM = do
    n <- get
    modify (+1)
    return n

newVar :: (Sunroof a) => CompM a
newVar = mkVar App.<$> uniqM

newLoop :: CompM String
newLoop = do
    u <- uniqM
    return $ "loop" ++ show u
