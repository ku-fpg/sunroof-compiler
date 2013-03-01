{-# LANGUAGE GADTs, RankNTypes, KindSignatures, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Language.Sunroof.Compiler
  ( compileJS
  , CompilerOpts(..)
  ) where

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


compileAST :: (Sunroof a) => CompilerOpts -> Uniq -> JS a -> IO (([Stmt], Expr), Uniq)
compileAST opts uq jsm = runStateT (runReaderT (compile jsm) opts) uq

compileJS :: (Sunroof a) => CompilerOpts -> Uniq -> JS a -> IO ((String, String), Uniq)
compileJS opts uq jsm = do
        ((stmts, res), u) <- compileAST opts uq jsm
        return ((unlines $ fmap showStmt stmts, showExpr False res), u)

-- compile an existing expression
compile :: Sunroof c => JS c -> CompM ([Stmt], Expr)
compile = eval . view . unJS
    -- since the type  Program  is abstract (for efficiency),
    -- we have to apply the  view  function first,
    -- to get something we can pattern match on
    where eval :: Sunroof b => ProgramView JSI b -> CompM ([Stmt], Expr)
          eval (JS_Eval e :>>= g) = do
            compileBind (unbox e) (JS . g)
          eval (JS_Assign (JSSelector sel) a obj :>>= g) = do
            -- note, this is where we need to optimize/CSE  the a value.
            -- TODO: this constructor should return unit, not the updated value
            (stmts0,val) <- compileExpr (unbox a)
            (stmts1,ret) <- compile $ JS (g ())
            return ( stmts0 ++ [AssignStmt (unbox obj) (unbox sel) val] ++ stmts1, ret )
          eval (JS_Select (JSSelector sel) obj :>>= g) = do
            compileBind (Apply (ExprE (Var "[]")) [ExprE $ unbox obj, ExprE $ unbox sel]) (JS . g)
          eval (JS_Invoke args fn :>>= g) = do
            compileBind (Apply (ExprE $ unbox fn) (map ExprE args)) (JS . g)
          eval (JS_Function fun :>>= g) = do
            e <- compileFunction fun
            compileBind e (JS . g)
          eval (JS_Branch b c1 c2 :>>= g) = do
            branch <- compileBranch b c1 c2
            compileStatement branch (JS . g)
          eval (JS_Foreach arr body :>>= g) = do
            loop <- compileForeach arr body
            compileStatement loop (JS . g)
          -- or we're done already
          eval (Return b) = compileExpr (unbox b)

compileBind :: forall a b . (Sunroof a, Sunroof b)
            => Expr -> (a -> JS b) -> CompM ([Stmt], Expr)
compileBind e m2 = do
    a <- newVar
    (stmts0, val) <- compileExpr e
    (stmts1,ret) <- compile (m2 a)
    return (stmts0 ++ [assignVar (Proxy::Proxy a) (varId a) val] ++ stmts1 , ret)

-- TODO: inline
compileStatement :: (Sunroof a, Sunroof b)
                 => ([Stmt], Expr) -> (a -> JS b) -> CompM ([Stmt], Expr)
compileStatement (stmts0, e) m2 = do
    (stmts,ret) <- compile $ m2 (box e)
    return (stmts0 ++ stmts , ret)

compileBranch :: forall a bool . (Sunroof a, Sunroof bool)
              => bool -> JS a -> JS a -> CompM ([Stmt], Expr)
compileBranch b c1 c2 = do
  (res :: a) <- newVar
  (src0, res0) <- compileExpr (unbox b)
  (src1, res1) <- compile c1
  (src2, res2) <- compile c2
  return ( src0 ++
           [ IfStmt res0 (src1 ++ [assignVar (Proxy::Proxy a) (varId res) res1])
                         (src2 ++ [assignVar (Proxy::Proxy a) (varId res) res2])
           ]
         , unbox res)

compileFunction :: forall a b . (JSArgument a, Sunroof b)
                => (a -> JS b) -> CompM Expr
compileFunction m2 = do
    (arg :: a) <- jsValue
    (fStmts,ret) <- compile (m2 arg)
    return $ Function (map varIdE $ jsArgs arg) (fStmts ++ [ReturnStmt ret])

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

