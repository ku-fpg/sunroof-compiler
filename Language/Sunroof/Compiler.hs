
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Sunroof.Compiler
  ( sunroofCompiler
  , compileJSI
  , extractProgram
  , CompilerOpts(..)
  ) where

import Control.Monad.Operational
import Control.Monad.State
import Control.Monad.Reader

import Data.Reify
import Data.Graph
import Data.Maybe
import Data.Proxy ( Proxy(..) )
import qualified Data.Map as Map
import Data.Default

import Language.Sunroof.Types 
  ( T(..)
  , JS(..), JSI(..)
  , JSThread(..), JSThreadReturn(..)
  , ThreadProxy(..)
  , single, apply, unJS )
import Language.Sunroof.JavaScript 
  ( Stmt(..), Id
  , E(..), ExprE(..), Expr
  , Type(..)
  , showStmt )
import Language.Sunroof.Classes 
  ( Sunroof(..), JSArgument(..)
  , UniqM(..), Uniq )
import Language.Sunroof.Selector ( unboxSelector )
import Language.Sunroof.Internal ( proxyOf )

-- -------------------------------------------------------------
-- Compiler
-- -------------------------------------------------------------

data CompilerOpts = CompilerOpts
  { co_on      :: Bool        -- do we reify to capture Haskell-level lets / CSEs?
  , co_cse     :: Bool        -- do we also capture non-reified CSE, using Value Numbering?
  , co_const   :: Bool        -- do we constant fold?
  , co_verbose :: Int         -- how verbose is the compiler when running? standard 0 - 3 scale
  }
  deriving Show

instance Default CompilerOpts where
  def = CompilerOpts True False False 0

sunroofCompiler :: (Sunroof a) => CompilerOpts -> String -> JS A a -> IO String
sunroofCompiler opts fName f = do
  (stmts,_) <- compileJSI opts 0 $ extractProgram (single . JS_Return) f
  return $ showStmt $ VarStmt fName $ Apply (ExprE $ Function [] stmts) []

extractProgram :: (a -> JS t ()) -> JS t a -> Program (JSI t) ()
extractProgram k m = unJS (m >>= k) return

-- TODO: generalize with a closer
compileJSI :: CompilerOpts -> Uniq -> Program (JSI t) () -> IO ([Stmt], Uniq)
compileJSI opts uq jsi_prog = runStateT (runReaderT (compile jsi_prog) opts) uq

compile :: Program (JSI t) () -> CompM [Stmt]
compile = eval . view
  -- since the type  Program  is abstract (for efficiency),
  -- we have to apply the  view  function first,
  -- to get something we can pattern match on
  where 
    eval :: ProgramView (JSI t) () -> CompM [Stmt]
    -- Return *will* be (), because of the normalization to CPS.
    eval (Return ()) = return []

    -- These are in the same order as the constructors.

    eval (JS_Eval e :>>= g) = do
      compileBind (unbox e) g

    eval (JS_Assign sel a obj :>>= g) = do
      -- note, this is where we need to optimize/CSE  the a value.
      -- TODO: this constructor should return unit, not the updated value
      (stmts0,val) <- compileExpr (unbox a)
      stmts1 <- compile (g ())
      return ( stmts0 ++ [AssignStmt (unbox obj) (unboxSelector sel) val] ++ stmts1)

      -- TODO: this is wrong : use Dot
    eval (JS_Select sel obj :>>= g) = do
      compileBind (Apply (ExprE (Var "[]")) [ExprE $ unbox obj, ExprE $ unboxSelector sel]) g

    -- Return returns Haskell type JS A (), because there is nothing after a return.
    -- We ignore everything after a return.
    eval (JS_Return e :>>= _) = do
      let ty = typeOf (proxyOf e)
      case ty of
        Unit -> return []                -- nothing to return
        _    -> do
          (stmts0,val) <- compileExpr (unbox e)
          return ( stmts0 ++ [ ReturnStmt val])

    -- All assignments to () are not done.
    eval (JS_Assign_ _ a :>>= g) | typeOf (proxyOf a) == Unit = do
      stmts1 <- compile (g ())
      return stmts1

    eval (JS_Assign_ v a :>>= g) = do
      (stmts0,val) <- compileExpr (unbox a)
      stmts1 <- compile (g ())
      return ( stmts0 ++ [AssignStmt_ (Var v) val] ++ stmts1)

    eval (JS_Invoke args fn :>>= g) = do
      compileBind (Apply (ExprE $ unbox fn) (map ExprE args)) g

    eval (JS_Function f :>>= g) = do
      e <- compileFunction f
      compileBind e g

    eval (JS_Branch b c1 c2 :>>= g) = compileBranch b c1 c2 g
    {-
    eval (JS_Foreach arr body :>>= g) = do
      loop <- compileForeach arr body
      rest <- compile (g ())
      return (loop ++ rest)
    -}


compileBind :: forall a t . (Sunroof a)
            => Expr
            -> (a -> Program (JSI t) ())
            -> CompM [Stmt]
compileBind e m2 = do
  a <- newVar
  (stmts0,val) <- compileExpr e
  stmts1       <- compile (m2 (var a))
  if (typeOf (Proxy::Proxy a) == Unit)
    then return (stmts0 ++ [ExprStmt val] ++ stmts1 )
    else return (stmts0 ++ [VarStmt a val] ++ stmts1 )

compileBranch_A :: forall a bool t . (Sunroof a, Sunroof bool)
                => bool -> JS t a -> JS t a ->  (a -> Program (JSI t) ()) -> CompM [Stmt]
compileBranch_A b c1 c2 k = do
  -- TODO: newVar should take a Id, or return an ID. varId is a hack.
  res          <- newVar
  (src0, res0) <- compileExpr (unbox b)
  src1 <- compile $ extractProgram (single . JS_Assign_ res) c1
  src2 <- compile $ extractProgram (single . JS_Assign_ res) c2
  rest <- compile (k (var res))
  return ( [VarStmt res (Var "undefined")] ++  src0 ++ [ IfStmt res0 src1 src2 ] ++ rest)

compileBranch_B :: forall a bool t . (Sunroof bool, JSArgument a, JSThread t)
                => bool -> JS t a -> JS t a ->  (a -> Program (JSI t) ()) -> CompM [Stmt]
compileBranch_B b c1 c2 k = do
  fn_e <- compileFunction (\ a -> JS $ \ k2 -> k a >>= k2)
  -- TODO: newVar should take a Id, or return an ID. varId is a hack.
  fn           <- newVar
  (src0, res0) <- compileExpr (unbox b)
  src1 <- compile $ extractProgram (apply (var fn)) c1
  src2 <- compile $ extractProgram (apply (var fn)) c2
  return ( [VarStmt fn fn_e] ++  src0 ++ [ IfStmt res0 src1 src2 ])

compileBranch :: forall a bool t . (JSThread t, Sunroof bool, Sunroof a, JSArgument a)
              => bool -> JS t a -> JS t a ->  (a -> Program (JSI t) ()) -> CompM [Stmt]
compileBranch b c1 c2 k = 
  case evalStyle (ThreadProxy :: ThreadProxy t) of
    A -> compileBranch_A b c1 c2 k
    B -> compileBranch_B b c1 c2 k

compileFunction :: forall a b t . (JSThreadReturn t b, JSArgument a, Sunroof b)
                => (a -> JS t b)
                -> CompM Expr
compileFunction m2 = do
  (arg :: a) <- jsValue
  fStmts <- compile $ extractProgram (\ a -> JS $ \ k -> threadCloser a >>= k) (m2 arg)
  return $ Function (map varIdE $ jsArgs arg) fStmts

{-
compileForeach :: forall a b t . (JSThread t, Sunroof a, Sunroof b)
               => JSArray a -> (a -> JS t b) -> CompM [Stmt]
compileForeach arr body | evalStyle (ThreadProxy :: ThreadProxy t) == A = do
  counter <- newVar
  -- Introduce a new name for the array, so a possible literal array
  -- is not reprinted for each access.
  arrVar <- newVar

  let condRet = unbox $ (var counter :: JSNumber) <* (var arrVar ! attr "length")
  bodyStmts <- compile $ extractProgram (const $ return ()) $ do
    e <- evaluate $ var arrVar ! label (cast (var counter :: JSNumber))
    _ <- body e
    return ()
  let incCounterStmts = [ VarStmt counter (unbox (var counter + 1 :: JSNumber)) ]
      loopStmts =
        [ VarStmt counter (unbox (0 :: JSNumber))
        , VarStmt arrVar  (unbox arr)
        -- Recalculate the condition, in case the loop changed it.
        , WhileStmt (condRet) (bodyStmts ++ incCounterStmts) ]
  return loopStmts
compileForeach _arr _body = error "compileForeach: Threading model wrong."
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
optExpr _opts e = do
  Graph g start <- liftIO $ reifyGraph (ExprE e)

  let db0 = Map.fromList [ (n,Inst e') | (n,e') <- g ]

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
                          Just (Inst oper) -> fmap (ExprE . findExpr vars db) oper
                          Just (Copy n') -> findExpr vars db n'
--                                Just op -> fmap (ExprE . findExpr db) op
                          Nothing -> error $ "optExpr: findExpr failed for " ++ show n

  -- replace dumb statement with better ones
  let folder :: (Ord n)
             => Map.Map n e
             -> [n]
             -> (e -> Map.Map n e -> Maybe e)
             -> Map.Map n e
      folder db [] _f = db
      folder db (n:ns) f = case Map.lookup n db of
                             Nothing -> error "bad folder"
                             Just e' -> case f e' db of
                                         Nothing -> folder db ns f
                                         Just e'' -> folder (Map.insert n e'' db) ns f

  let db1 = folder db0 ids $ \ e' db ->
               let --getExpr :: Uniq -> Expr
                   --getExpr = findExpr Map.empty db

                   getVar :: Uniq -> Maybe String
                   getVar expr = case findExpr jsVars db expr of { Var x -> return x ; _ -> Nothing }

                   getLit :: Uniq -> Maybe String
                   getLit expr = case findExpr Map.empty db expr of { Lit x -> return x ; _ -> Nothing }

               in case e' of
                    -- var c4770 = 0.0<=0.0;
                    (Inst (Apply g' [x,y])) | getVar g' == Just "<="
                                 && isJust (getLit x)
                                 && isJust (getLit y)
                                 && getLit x == getLit y
                                 -> return (Inst (Lit "true"))
                       -- var c4770 = true;
                       -- var c4771 = c4770?0.0:0.0;
                    Inst (Apply g' [x,y,z])
                          | getVar g' == Just "?:" && getLit x == return "true"
                                 -> return (Copy y)
                          | getVar g' == Just "?:" && getLit x == return "false"
                                 -> return (Copy z)
                    _ -> Nothing
  -- Next, do a forward push of value numbering
  let dbF = db1

  _ <- return undefined -- ???
  return ([ VarStmt c $ case e' of
                          Inst expr -> fmap (ExprE . findExpr jsVars dbF) expr
                          Copy n'   -> -- Apply (ExprE (Var "COPY")) [ ExprE $ findExpr jsVars dbF n' ]
                                       findExpr jsVars dbF n'
          | n <- ids
          , Just c    <- return $ Map.lookup n jsVars
          , Just e' <- return $ Map.lookup n dbF
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

newVar :: CompM Id
newVar = uniqM >>= return . ("v" ++) . show

--varId :: Sunroof a => a -> Id
--varId = varIdE . unbox

var :: Sunroof a => Id -> a
var = box . Var

varIdE :: Expr -> Id
varIdE e = case e of
  (Var v) -> v
  v -> error $ "varId: Expressions is not a variable: " ++ show v

----------------------------------------------------------------------------------

