{-# LANGUAGE GADTs, RankNTypes, KindSignatures, DataKinds, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Language.Sunroof.Compiler
--  ( compileJS
--  , CompilerOpts(..)
--  ) where
        where

--import qualified Control.Applicative as App
import Control.Monad.Operational
import Control.Monad.State
import Control.Monad.Reader
--import Data.List (intercalate)

import Language.Sunroof.Types
import Data.Reify
import Data.Graph
import Data.Maybe
import qualified Data.Map as Map
import Data.Default
import Data.Proxy
import Data.Boolean
import Debug.Trace
--import Web.KansasComet (Template(..), extract)

data CompilerOpts = CompilerOpts
        { co_on      :: Bool        -- do we reify to capture Haskell-level lets / CSEs?
        , co_cse     :: Bool        -- do we also capture non-reified CSE, using Value Numbering?
        , co_const   :: Bool        -- do we constant fold?
        , co_verbose :: Int         -- how verbose is the compiler when running? standard 0 - 3 scale
        }

instance Default CompilerOpts where
        def = CompilerOpts True False False 0

compileJS_A :: (Sunroof a) => CompilerOpts -> Uniq -> JS A a -> IO ([Stmt], Uniq)
compileJS_A opts uq = compileJSI opts uq . extractProgram (JS_ . singleton . JS_Return)

-- It is not possible to compile JS B a, because where does the 'a' get returned to?
compileJS_B :: (Sunroof a) => CompilerOpts -> Uniq -> JS B () -> IO ([Stmt], Uniq)
compileJS_B opts uq = compileJSI opts uq . extractProgram (const $ return ())

extractProgram :: (a -> JS t ()) -> JS t a -> Program (JSI t) ()
extractProgram k m = case (m >>= k) of
                       JS f -> f return
                       JS_ p -> p

-- TODO: generalize with a closer
compileJSI :: CompilerOpts -> Uniq -> Program (JSI t) () -> IO ([Stmt], Uniq)
compileJSI opts uq jsi_prog = runStateT (runReaderT (compile jsi_prog) opts) uq

compile :: Program (JSI t) () -> CompM [Stmt]
compile = eval . view
    -- since the type  Program  is abstract (for efficiency),
    -- we have to apply the  view  function first,
    -- to get something we can pattern match on
    where eval :: ProgramView (JSI t) () -> CompM [Stmt]
          -- Return *will* be (), because of the normalization to CPS.
          eval (Return ()) = return []

          -- These are in the same order as the constructors.

          eval (JS_Eval e :>>= g) = do
            compileBind (unbox e) g

          eval (JS_Assign (JSSelector sel) a obj :>>= g) = do
            -- note, this is where we need to optimize/CSE  the a value.
            -- TODO: this constructor should return unit, not the updated value
            (stmts0,val) <- compileExpr (unbox a)
            stmts1 <- compile (g ())
            return ( stmts0 ++ [AssignStmt (unbox obj) (unbox sel) val] ++ stmts1)

            -- TODO: this is wrong : use Dot
          eval (JS_Select (JSSelector sel) obj :>>= g) = do
            compileBind (Apply (ExprE (Var "[]")) [ExprE $ unbox obj, ExprE $ unbox sel]) g

          -- Return returns Haskell type JS A (), because there is nothing after a return.
          -- We ignore everything after a return.
          eval (JS_Return e :>>= _) = do
            let ty = typeOf e
            case ty of
               Unit -> return []                -- nothing to return
               _    -> do
                  (stmts0,val) <- compileExpr (unbox e)
                  return ( stmts0 ++ [ ReturnStmt val])

          eval (JS_Assign_ var a :>>= g) = do
            (stmts0,val) <- compileExpr (unbox a)
            stmts1 <- compile (g ())
            return ( stmts0 ++ [AssignStmt_ (Var var) val] ++ stmts1)

          eval (JS_Invoke args fn :>>= g) = do
            compileBind (Apply (ExprE $ unbox fn) (map ExprE args)) g

          eval (JS_Function fun :>>= g) = do
            e <- compileFunction fun
            compileBind e g

          eval (JS_Branch b c1 c2 :>>= g) = do
            (branch,res) <- compileBranch_A b c1 c2
            stmt1 <- compile (g res)
            return $ branch ++ stmt1

{-
          eval (JS_Foreach arr body :>>= g) = do
            loop <- compileForeach arr body
            compileStatement loop (JS_ . g)
          -- or we're done already
          eval (Return b) = compileExpr (unbox b)
-}


compileBind :: (Sunroof a)
            => Expr
            -> (a -> Program (JSI t) ())
            -> CompM [Stmt]
compileBind e m2 = do
    a <- newVar
    (stmts0,val) <- compileExpr e
    stmts1       <- compile (m2 a)
    return (stmts0 ++ [VarStmt (varId a) val] ++ stmts1)
{-
-- TODO: inline
compileStatement :: (Sunroof a, Sunroof b)
                 => ([Stmt], Expr) -> (a -> JS b) -> CompM ([Stmt], Expr)
compileStatement (stmts0, e) m2 = do
    (stmts,ret) <- compile $ m2 (box e)
    return (stmts0 ++ stmts , ret)
-}

compileBranch_A :: forall a bool . (Sunroof a, Sunroof bool)
              => bool -> JS A a -> JS A a -> CompM ([Stmt],a)
compileBranch_A b c1 c2 = do
  -- TODO: newVar should take a Id, or return an ID. varId is a hack.
  (res :: a)   <- newVar
  (src0, res0) <- compileExpr (unbox b)
  src1 <- compile $ extractProgram (JS_ . singleton . JS_Assign_ (varId res)) c1
  src2 <- compile $ extractProgram (JS_ . singleton . JS_Assign_ (varId res)) c1
  return ( [VarStmt (varId res) (Var "undefined")] ++  src0 ++ [ IfStmt res0 src1 src2 ], res)

compileFunction :: forall a b t . (JSThread t b, JSArgument a, Sunroof b)
                => (a -> JS t b)
                -> CompM Expr
compileFunction m2 = do
    (arg :: a) <- jsValue
    fStmts <- compile $ extractProgram (JS_ . threadCloser) (m2 arg)
    return $ Function (map varIdE $ jsArgs arg) fStmts

{-
compileForeach :: forall a b . (Sunroof a, Sunroof b)
               => JSArray a -> (a -> JS b) -> CompM ([Stmt], Expr)
compileForeach arr body = do
  (counter :: JSNumber) <- newVar
  -- Introduce a new name for the array, so a possible literal array
  -- is not reprinted for each access.
  (arrVar :: JSArray a) <- newVar
  (condStmts, condRet) <- compile $ do
    return $ counter <* (cast arrVar ! attribute "length")
  (bodyStmts, bodyRet) <- compile $ do
    _ <- body (cast arrVar ! label (cast counter))
    return ()
  let incCounterStmts =
        [ VarStmt (varId counter) (unbox (counter + 1 :: JSNumber)) ]
      loopStmts =
        [ VarStmt (varId counter) (unbox (0 :: JSNumber))
        , VarStmt (varId arrVar)  (unbox arr) ]
        ++ condStmts ++
        -- Recalculate the condition, in case the loop changed it.
        [ WhileStmt (condRet) (bodyStmts ++ condStmts ++ incCounterStmts) ]
  return (loopStmts, bodyRet)

-- turn an expression into a list of statements, followed by an expression.
-- allows for CSE inside Expr
-}

compileExpr :: Expr -> CompM ([Stmt], Expr)
compileExpr e = do
        opts <- ask
        optExpr opts e


data Inst i e = Inst (i e)
              | Copy e          -- an indirection
   deriving (Show)

optExpr :: CompilerOpts -> Expr -> CompM ([Stmt], Expr)
optExpr opts e | not (co_on opts) = return ([],e)
optExpr opts e = do

        Graph g start <- liftIO $ reifyGraph (ExprE e)

        liftIO $ print (g,start)

        let db0 = Map.fromList [ (n,Inst e) | (n,e) <- g ]

        let out = stronglyConnComp
                        [ (n,n,case e' of
                                Apply f xs -> f : xs
                                _ -> [])
                        | (n,e') <- g
                        ]


        let ids = filter (/= start) $ flattenSCCs $ out


        jsVars :: Map.Map Uniq String <- liftM Map.fromList $ sequence
                    [ do v <- uniqM
                         return (n,"c" ++ show v)
                    | n <- ids
                    , Just (Inst (Apply {})) <- [ Map.lookup n db0 ]
                    ]

        let findExpr vars db n =
              case Map.lookup n vars of
                  Just v -> Var v
                  Nothing  -> case Map.lookup n db of
                                Just (Inst op) -> fmap (ExprE . findExpr vars db) op
                                Just (Copy n') -> findExpr vars db n'
--                                Just op -> fmap (ExprE . findExpr db) op
                                Nothing -> error $ "optExpr: findExpr failed for " ++ show n

        -- replace dumb statement with better ones
        let folder :: (Ord n)
                   => Map.Map n e
                   -> [n]
                   -> (e -> Map.Map n e -> Maybe e)
                   -> Map.Map n e
            folder db [] f = db
            folder db (n:ns) f = case Map.lookup n db of
                                   Nothing -> error "bad folder"
                                   Just e -> case f e db of
                                               Nothing -> folder db ns f
                                               Just e' -> folder (Map.insert n e' db) ns f


        let db1 = folder db0 ids $ \ e db ->
                     let getExpr :: Uniq -> Expr
                         getExpr = findExpr Map.empty db

                         getVar :: Uniq -> Maybe String
                         getVar e = case findExpr jsVars db e of { Var x -> return x ; _ -> Nothing }

                         getLit :: Uniq -> Maybe String
                         getLit e = case findExpr Map.empty db e of { Lit x -> return x ; _ -> Nothing }

                     in case e of
                             -- var c4770 = 0.0<=0.0;
                          (Inst (Apply g [x,y])) | getVar g == Just "<="
                                       && isJust (getLit x)
                                       && isJust (getLit y)
                                       && getLit x == getLit y
                                       -> return (Inst (Lit "true"))
                             -- var c4770 = true;
                             -- var c4771 = c4770?0.0:0.0;

                          Inst (Apply g [x,y,z])
                                | getVar g == Just "?:" && getLit x == return "true"
                                       -> return (Copy y)
                                | getVar g == Just "?:" && getLit x == return "false"
                                       -> return (Copy z)

                          _ -> Nothing

--                      getVar :: Uniq -> String
--                      getLit :: Uniq -> String


                                -- c4773?0.0:0.0;
--                      f (Apply g xs)
--                          | g

        -- Next, do a forward push of value numbering
        let dbF = db1

        return undefined
        return ([ VarStmt c $ case e of
                                Inst expr -> fmap (ExprE . findExpr jsVars dbF) expr
                                Copy n'   -> -- Apply (ExprE (Var "COPY")) [ ExprE $ findExpr jsVars dbF n' ]
                                             findExpr jsVars dbF n'
                | n <- ids
                , Just c    <- return $ Map.lookup n jsVars
                , Just e <- return $ Map.lookup n dbF
                ], findExpr jsVars dbF start)
--        return ([],e)

compilerLog :: Int -> String -> CompM ()
compilerLog level msg = do
  opts <- ask
  when (co_verbose opts >= level) $ liftIO $ do
      putStr "Compiler: "
      putStrLn msg


-----------------------------------------------------------------------------------

type CompM = ReaderT CompilerOpts (StateT Uniq IO)

instance UniqM CompM where
  uniqM = do
    n <- get
    modify (+1)
    return n

newVar :: (Sunroof a) => CompM a
newVar = jsVar

varId :: Sunroof a => a -> Id
varId = varIdE . unbox

varIdE :: Expr -> Id
varIdE e = case e of
  (Var v) -> v
  v -> error $ "varId: Expressions is not a variable: " ++ show v

----------------------------------------------------------------------------------

testA :: (Sunroof a) => JS A a -> IO [Stmt]
testA m = do
  (stmts,_) <- compileJS_A def 0 m
  return stmts

