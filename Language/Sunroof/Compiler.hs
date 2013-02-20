{-# LANGUAGE GADTs, RankNTypes, KindSignatures, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Language.Sunroof.Compiler where

--import qualified Control.Applicative as App
import Control.Monad.Operational
import Control.Monad.State
import Data.List (intercalate)

import Language.Sunroof.Types
--import Web.KansasComet (Template(..), extract)

compileJS :: (Sunroof a) => JS a -> (String,String)
compileJS = fst . compileJS' 0

compileJS' :: (Sunroof a) => Uniq -> JS a -> ((String, String), Uniq)
compileJS' uq jsm = runState (compile jsm) uq

-- compile an existing expression
compile :: Sunroof c => JS c -> CompM (String,String)
compile = eval . view . unJS
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
            compileBind "" ("(" ++ showVar o ++ ")") (JS . g)
          eval (JS_Assign sel a obj :>>= g) = do
            compileBind "" ("(" ++ showVar obj ++ ")[" ++ show sel ++ "] = (" ++ show (unbox a) ++ ");") (JS . g)
          eval (JS_Select sel obj :>>= g) = do
             -- if a function, then wrap with
             -- function(v){return document.getElementById(v);}
            compileBind "" ("(" ++ showVar obj ++ ")[" ++ show sel ++ "]") (JS . g)
          eval (JS_Invoke args fn :>>= g) = do
            compileBind "" ("(" ++ showVar fn ++ ")(" ++ intercalate "," (map show args) ++ ")") (JS . g)
{-
compileAction o (Invoke args) =
        return ("","(" ++ showVar o ++ ")(" ++ intercalate "," (map show args) ++ ")")
compileAction o (JSSelector nm := val) =
                                -- this is a total hack, (pres. is wrong), but works
        return ("","(" ++ showVar o ++ ")[" ++ show nm ++ "] = (" ++ show (unbox val) ++ ")")


          eval (JS_App o jss :>>= g) = do
            (pre,act) <- compileAction o jss
            compileBind pre act (JS . g)

    JS_Assign  :: (Sunroof a) => JSSelector a -> a -> JSObject  -> JSI ()

    JS_Select  :: (Sunroof a) => JSSelector a -> JSObject      -> JSI a

    JS_Invoke :: (Sunroof r) => [Expr] -> JSFunction a r        -> JSI r        -- Perhaps take the overloaded vs [Expr]
                                                                                -- and use jsArgs in the compiler?


-}
--            ("(" ++ showVar o ++ ")" ++ sel_txt) g
--          eval (JS_Invoke args :>>= g) = do
--            compileBind ("(function(o) { return o.(" ++ intercalate "," (map show args) ++ ");}") g

          eval (JS_Function fun :>>= g) = do
            txt1 <- compileFunction fun
            compileBind "" txt1 (JS . g)
          eval (JS_Branch b c1 c2 :>>= g) = do
            branch <- compileBranch b c1 c2
            compileCommand branch (JS . g)
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

compileBind :: (Sunroof a, Sunroof b) => String -> String -> (a -> JS b) -> CompM (String,String)
compileBind txt0 txt1 m2 = do
    a <- newVar
    (txt2,ret) <- compile (m2 a)
    return (txt0 ++ assignVar a txt1 ++ txt2,ret)

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
                   , src1, assignVar res res1
                   , "} else {"
                   , src2, assignVar res res2
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
{-
compileAction :: (Sunroof a, Sunroof b) => a -> Action a b -> CompM (String,String)
compileAction o (Invoke args) =
        return ("","(" ++ showVar o ++ ")(" ++ intercalate "," (map show args) ++ ")")
compileAction o (JSSelector nm := val) =
                                -- this is a total hack, (pres. is wrong), but works
        return ("","(" ++ showVar o ++ ")[" ++ show nm ++ "] = (" ++ show (unbox val) ++ ")")
compileAction o (Map f act) =
        compileAction (f o) act
compileAction o (NoAction b) =
        return ("",showVar b)
compileAction o (BindAction (NoAction a) k) = compileAction o (k a)
compileAction o (BindAction m@(Invoke {}) k) = compileAction' o m k
compileAction o (BindAction m@(_ := _) k) = compileAction' o m k
compileAction o (BindAction m@(Map {}) k) = compileAction' o m k
compileAction o (BindAction (BindAction m k1) k2) = compileAction o (BindAction m (\ a -> BindAction (k1 a) k2))

compileAction' :: (Sunroof a, Sunroof b, Sunroof c) => a -> Action a b -> (b -> Action a c) => CompM (String,String)
compileAction' o m k = do
        (p1,v1) <- compileAction o m
        a <- newVar
        (p2,v2) <- compileAction o (k a)
        return (p1 ++ ";" ++ assignVar a ++ v1 ++ ";" ++ p2, v2)

--        return $ showVar b
--   BindAction :: Action a b -> (b -> Action a c)                -> Action a c
-}
{-
compileAction o (Multi many) = do
        patches <- mapM (compileAction o) many
        return $ intercalate ";" patches
-}

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
