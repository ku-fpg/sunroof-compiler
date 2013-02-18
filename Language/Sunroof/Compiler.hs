{-# LANGUAGE GADTs, RankNTypes, KindSignatures, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Language.Sunroof.Compiler where

import qualified Control.Applicative as App
import Control.Monad.Operational
import Control.Monad.State
import Data.List (intercalate)

import Language.Sunroof.Types
import Web.KansasComet (Template(..), extract)

compileJS :: (Sunroof a) => JS a -> (String,String)
compileJS = flip evalState 0 . compile

-- compile an existing expression
compile :: Sunroof c => JS c -> CompM (String,String)
compile = eval . view
    -- since the type  Program  is abstract (for efficiency),
    -- we have to apply the  view  function first,
    -- to get something we can pattern match on
    where eval :: Sunroof b => ProgramView JSI b -> CompM (String,String)
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
          eval (JS_Branch b c1 c2 :>>= g) = do
            branch <- compileBranch b c1 c2
            compileCommand branch g
{-
          eval (JS_Loop body :>>= g) = do -- note, we do nothing with g, as it's unreachable
            -- create a new name for our loop
            loop <- newLoop
            -- the magic: add call to loop at end of loop body instructions,
            -- this way, if body contains a JS_Wait, it gets sucked into the
            -- event callback!
            loop_body <- compile (body >>= (\() -> singleton (JS_App (box $ Lit loop) $ Invoke [] :: JSI ())))
            -- define loop function, and call it once
            return ("var " ++ loop ++ " = function(){ " ++ loop_body ++ " }; " ++ loop ++ "();", "ERROR")
-}
{-
          eval (JS_Wait tmpl k :>>= g) = do
            txt1 <- compileFunction fun
            a <- newVar
            txt2 <- compile (g a)
            let eventNames = map fst $ extract tmpl
            return $ "$.kc.waitFor(" ++ show eventNames ++ ",function(" ++ showVar a ++ "){" ++ txt2 ++ "})"
-}
          -- or we're done already
          eval (Return b) = return ("",showVar b)

compileBind :: (Sunroof a, Sunroof b) => String -> (a -> JS b) -> CompM (String,String)
compileBind txt1 m2 = do
    a <- newVar
    (txt2,ret) <- compile (m2 a)
    return (assignVar a ++ txt1 ++ ";" ++ txt2,ret)

-- Does the same as 'compileBind' but does not bind the result of the passed in
-- JavaScript source to a variable. Like this control flow constructs like
-- branches can be translated.
compileCommand :: (Sunroof a, Sunroof b) => (String, a) -> (a -> JS b) -> CompM (String, String)
compileCommand (src1, comRes) f = do
    (src2,ret) <- compile (f comRes)
    return (src1 ++ ";" ++ src2, ret)

compileBranch :: (Sunroof a, Sunroof bool) => bool -> JS a -> JS a -> CompM (String, a)
compileBranch b c1 c2 = do
  res <- newVar
  (src1, res1) <- compile c1
  (src2, res2) <- compile c2
  return $ (concat [ "if(", showVar b, ") {"
                   , src1, assignVar res, res1, ";"
                   , "} else {"
                   , src2, assignVar res, res2, ";"
                   , "}" ]
           , res)

compileFunction :: forall a b . (JSArgument a, Sunroof b) => (a -> JS b) -> CompM String
compileFunction m2 = do
    start    <- get
    arg :: a <- jsValue
    end      <- get

    (txt2,ret) <- compile (m2 arg)

    let arg_list = intercalate "," $ map (show . Var) $ [start..(end - 1)]

    return $ "(function (" ++ arg_list ++ "){" ++ txt2 ++ "; return " ++ ret ++ ";})"

-- These are a mix of properties, methods, and assignment.
-- What is the unifing name? JSProperty?

compileAction :: (Sunroof a) => a -> Action a b -> CompM String
compileAction o (Invoke args) =
        return $ "(" ++ showVar o ++ ")(" ++ intercalate "," (map show args) ++ ")"
compileAction o (JSSelector nm := val) =
        return $ "(" ++ showVar o ++ ")[" ++ show nm ++ "] = (" ++ show (unbox val) ++ ")" -- this is a total hack, (pres. is wrong), but works
compileAction o (Map f act) =
        compileAction (f o) act



type CompM = State Uniq

instance UniqM CompM where
  uniqM = do
    n <- get
    modify (+1)
    return n

newVar :: (Sunroof a) => CompM a
newVar = jsVar

newLoop :: CompM String
newLoop = do
    u <- uniqM
    return $ "loop" ++ show u
