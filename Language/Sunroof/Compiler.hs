{-# LANGUAGE GADTs, RankNTypes, KindSignatures #-}
module Language.Sunroof.Compiler where

import qualified Control.Applicative as App
import Control.Monad.Operational
import Control.Monad.State
import Data.List (intercalate)

import Language.Sunroof.Types
import Web.KansasComet (Template(..), extract)

compileJS :: (Sunroof a) => JS a -> String
compileJS = flip evalState 0 . compile

-- compile an existing expression
compile :: Sunroof c => JS c -> CompM String
compile = eval . view
    -- since the type  Program  is abstract (for efficiency),
    -- we have to apply the  view  function first,
    -- to get something we can pattern match on
    where eval :: Sunroof b => ProgramView JSI b -> CompM String
          -- either we call a primitive JavaScript function
--          eval (JS_Select jss :>>= g) = do
--            txt1 <- compileJSS jss
--            compileBind txt1 g
--          eval (JS_Dot o jss :>>= g) = do
--            sel_txt <- compileJSS jss
--            compileBind ("(" ++ showVar o ++ ")." ++ sel_txt) g
          eval (JS_Eval o :>>= g) = do
            compileBind ("(" ++ showVar o ++ ")") g
          eval (JS_App o jss :>>= g) = do
            act <- compileAction o jss
            compileBind act g
--            ("(" ++ showVar o ++ ")" ++ sel_txt) g
--          eval (JS_Invoke args :>>= g) = do
--            compileBind ("(function(o) { return o.(" ++ intercalate "," (map show args) ++ ");}") g

          eval (JS_Function fun :>>= g) = do
            txt1 <- compileFunction fun
            compileBind txt1 g
          eval (JS_Loop body :>>= g) = do -- note, we do nothing with g, as it's unreachable
            -- create a new name for our loop
            loop <- newLoop
            -- the magic: add call to loop at end of loop body instructions,
            -- this way, if body contains a JS_Wait, it gets sucked into the
            -- event callback!
            loop_body <- compile (body >>= (\() -> singleton (JS_App (box $ Lit loop) $ Invoke [] :: JSI ())))
            -- define loop function, and call it once
            return $ "var " ++ loop ++ " = function(){ " ++ loop_body ++ " }; " ++ loop ++ "();"
{-
          eval (JS_Wait tmpl k :>>= g) = do
            txt1 <- compileFunction fun
            a <- newVar
            txt2 <- compile (g a)
            let eventNames = map fst $ extract tmpl
            return $ "$.kc.waitFor(" ++ show eventNames ++ ",function(" ++ showVar a ++ "){" ++ txt2 ++ "})"
-}
          -- or we're done already
          eval (Return b) = return $ showVar b

compileBind :: (Sunroof a, Sunroof b) => String -> (a -> JS b) -> CompM String
compileBind txt1 m2 = do
    a <- newVar
    txt2 <- compile (m2 a)
    return $ assignVar a ++ txt1 ++ ";" ++ txt2

compileFunction :: (Sunroof a, Sunroof b) => (a -> JS b) -> CompM String
compileFunction m2 = do
    a <- newVar
    txt2 <- compile (m2 a)
    -- continuation problem (if you have a continuation, then this will go wrong)
    return $ "(function (" ++ showVar a ++ "){" ++ txt2 ++ "})"


-- These are a mix of properties, methods, and assignment.
-- What is the unifing name? JSProperty?

compileAction :: (Sunroof a) => a -> Action a b -> CompM String
compileAction o (Invoke args) =
        return $ "(" ++ showVar o ++ ")(" ++ intercalate "," (map show args) ++ ")"
compileAction o (nm := val) =
        return $ "(" ++ showVar o ++ ")[" ++ show nm ++ "] = (" ++ show (unbox val) ++ ")" -- this is a total hack, (pres. is wrong), but works
compileAction o (Map f act) =
        compileAction (f o) act



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