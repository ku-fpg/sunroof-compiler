
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Provides the Sunroof to Javascript compiler.
module Language.Sunroof.Compiler
  ( sunroofCompileJSA
  , sunroofCompileJSB
  , compileJS
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
  , SunroofThread(..)
  , ThreadProxy(..)
  , single, apply, unJS, nullJS
  , continuation, goto )
import Language.Sunroof.JavaScript
import Language.Sunroof.Classes
  ( Sunroof(..), SunroofArgument(..)
  , UniqM(..), Uniq )
import Language.Sunroof.Selector ( unboxSelector, (!) )
import Language.Sunroof.Internal ( proxyOf )

import Language.Sunroof.JS.Object ( JSObject )

-- -------------------------------------------------------------
-- Compiler
-- -------------------------------------------------------------

-- | Options to setup the compiler.
data CompilerOpts = CompilerOpts
  { co_on      :: Bool
    -- ^ Do we reify to capture Haskell-level lets / CSEs?
  , co_cse     :: Bool
    -- ^ Do we also capture non-reified CSE, using Value Numbering?
  , co_const   :: Bool
    -- ^ Do we constant fold?
  , co_verbose :: Int
    -- ^ How verbose is the compiler when running? standard 0 - 3 scale
  , co_compress :: Bool
      -- ^ Does the compiler output code without whitespace and layout? default == False
  }
  deriving Show

-- | Default compiler options.
instance Default CompilerOpts where
  def = CompilerOpts True False False 0 False

-- | The sunroof compiler compiles an effect that returns a Sunroof/JavaScript
-- value into a JavaScript program. An example invocation is
--
-- @
-- GHCi> import Language.Sunroof
-- GHCi> import Language.Sunroof.JS.Browser
-- GHCi> import Data.Default
-- GHCi> txt <- sunroofCompileJSA def \"main\" $ do alert(js \"Hello\");
-- GHCi> putStrLn txt
-- var main = (function() {
--   alert(\"Hello\");
-- })();
-- @
--
-- (The extra function and application are intentional and are a common JavaScript
-- trick to circumvent scoping issues.)
--
-- To generate a function, not just an effect, you can use the 'function' combinator.
--
-- @
-- GHCi> txt <- sunroofCompileJSA def \"main\" $ do
--            function $ \\ n -> do
--                return (n * (n :: JSNumber))
-- GHCi> putStrLn txt
-- var main = (function() {
--   var v1 = function(v0) {
--     return v0*v0;
--   };
--   return v1;
-- })();
-- @
--
-- Now @main@ in JavaScript is bound to the square function.
--
sunroofCompileJSA :: (Sunroof a) => CompilerOpts -> String -> JS A a -> IO String
sunroofCompileJSA opts fName f = do
  (stmts,_) <- compileJS opts 0 (single . JS_Return) f
  return $ showStmt $ mkVarStmt fName $ scopeForEffect stmts

-- | Compiles code using the blocking threading model.
--   Usage is the same as for 'sunroofCompileJSA'.
sunroofCompileJSB :: CompilerOpts -> String -> JS B () -> IO String
sunroofCompileJSB opts fName f = sunroofCompileJSA opts fName $ do
  k <- continuation (\ () -> f)
  goto k () :: JS A ()

-- | Extracts the 'Control.Monad.Operational.Program' from the given
--   Javascript computation using the given continuation closer.
extractProgramJS :: (a -> JS t ()) -> JS t a -> Program (JSI t) ()
extractProgramJS k m = unJS (m >>= k) return

-- | Compile a Javascript computation (using the given continuation closer)
--   into basic Javascript statements. Also return the next fresh
--   unique. This function should only be used if you know what your doing!
compileJS :: CompilerOpts -> Uniq -> (a -> JS t ()) -> JS t a -> IO ([Stmt], Uniq)
compileJS opts uq k m = runStateT (runReaderT (compile $ extractProgramJS k m) opts) uq

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
      --let ty = typeOf (proxyOf a)
      stmts1 <- compile (g ())
      return ( stmts0 ++ [AssignStmt (DotRhs (unbox obj) (unboxSelector sel)) val] ++ stmts1)

    eval (JS_Select sel obj :>>= g) = do
      compileBind (Apply (ExprE (Var "[]")) [ExprE $ unbox obj, ExprE $ unboxSelector sel]) g

    eval (JS_Delete sel obj :>>= g) = do
      let ty = typeOf (proxyOf (obj ! sel))
      stmts1 <- compile (g ())
      return (DeleteStmt (Dot (ExprE $ unbox obj) (ExprE $ unboxSelector sel) ty) : stmts1)

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
      return ( stmts0 ++ [AssignStmt (VarRhs v) val] ++ stmts1)

    eval (JS_Invoke args fn :>>= g) = do
      compileBind (Apply (ExprE $ unbox fn) (map ExprE (jsArgs args))) g

    eval (JS_Function f :>>= g) = do
      e <- compileFunction f
      compileBind e g

    eval (JS_Continuation f :>>= g) = do
      e <- compileContinuation f
      compileBind e g

    eval (JS_Branch b c1 c2 :>>= g) = compileBranch b c1 c2 g

    eval (JS_Fix h1 :>>= g) = compileFix h1 g

    eval (JS_Comment msg :>>= g) = do
      rest <- compile (g ())
      return $ CommentStmt msg : rest

compileBind :: forall a t . (Sunroof a)
            => Expr
            -> (a -> Program (JSI t) ())
            -> CompM [Stmt]
compileBind e m2 = do
  a <- newVar
  (stmts0,val) <- compileExpr e
  stmts1       <- compile (m2 (var a))
  let isUnit   = typeOf (Proxy::Proxy a) == Unit
      valIsTriv = case val of
                    Var {} -> True
                    Lit {} -> True
                    _      -> False
  case () of
   _ | isUnit && null stmts0 && valIsTriv
                 -> return stmts1
     | isUnit    -> return (stmts0 ++ [ExprStmt val] ++ stmts1 )
     | otherwise -> return (stmts0 ++ [mkVarStmt a val] ++ stmts1 )

compileBranch_A :: forall a bool t . (Sunroof a, Sunroof bool)
                => bool -> JS t a -> JS t a ->  (a -> Program (JSI t) ()) -> CompM [Stmt]
compileBranch_A b c1 c2 k = do
  -- TODO: newVar should take a Id, or return an ID. varId is a hack.
  res          <- newVar
  (src0, res0) <- compileExpr (unbox b)
  src1 <- compile $ extractProgramJS (single . JS_Assign_ res) c1
  src2 <- compile $ extractProgramJS (single . JS_Assign_ res) c2
  rest <- compile (k (var res))
  return (src0 ++ [ IfStmt res0 src1 src2 ] ++ rest)

compileBranch_B :: forall a bool t . (Sunroof bool, SunroofArgument a, SunroofThread t)
                => bool -> JS t a -> JS t a ->  (a -> Program (JSI t) ()) -> CompM [Stmt]
compileBranch_B b c1 c2 k = do
  fn_e <- compileContinuation (\ a -> blockableJS $ JS $ \ k2 -> k a >>= k2)
  -- TODO: newVar should take a Id, or return an ID. varId is a hack.
  fn           <- newVar
  (src0, res0) <- compileExpr (unbox b)
  src1 <- compile $ extractProgramJS (apply (var fn)) c1
  src2 <- compile $ extractProgramJS (apply (var fn)) c2
  return ( [mkVarStmt fn fn_e] ++  src0 ++ [ IfStmt res0 src1 src2 ])

compileBranch :: forall a bool t . (SunroofThread t, Sunroof bool, Sunroof a, SunroofArgument a)
              => bool -> JS t a -> JS t a ->  (a -> Program (JSI t) ()) -> CompM [Stmt]
compileBranch b c1 c2 k =
  case evalStyle (ThreadProxy :: ThreadProxy t) of
    A -> compileBranch_A b c1 c2 k
    B -> compileBranch_B b c1 c2 k

compileFix :: forall a t . (SunroofArgument a)
              => (a -> JS A a) ->  (a -> Program (JSI t) ()) -> CompM [Stmt]
compileFix h1 k = do
        -- invent the scoped named variables
        args <- jsValue
        -- set up the variables with null
        let initial =
                [ mkVarStmt v (unbox nullJS)
                | Var v <- jsArgs args
                ]

        body <- compile (unJS (h1 args) (\ res -> do
                when (length (jsArgs args) /= length (jsArgs res)) $
                        error "fatal error in mdo compile"
                singleton $ JS_Comment
                          $ "tie the knot"
                sequence_ [ singleton $ JS_Assign_ v (box $ e :: JSObject)
                          | (Var v, e) <- jsArgs args `zip` jsArgs res
                          ]))

        rest <- compile (k args)

        return $
                [ CommentStmt "set up recusive values" ] ++
                initial ++
                [ CommentStmt "body of the mdo-style rec" ] ++
                body ++
                [ CommentStmt "and proceed with the rest of the program"] ++
                rest

{-
unJS :: JS t a -> (a -> Program (JSI t) ()) -> Program (JSI t) ()
        :: (a -> JS t ()) -> JS t a -> Program (JSI t) ()
extractProgramJS k m = unJS (m >>= k) return



-}

-- var v =


compileFunction :: forall a b . (SunroofArgument a, Sunroof b)
                => (a -> JS A b)
                -> CompM Expr
compileFunction m2 = do
  (arg :: a) <- jsValue
  fStmts <- compile $ extractProgramJS (\ a -> JS $ \ k -> singleton (JS_Return a) >>= k) (m2 arg)
  return $ Function (map varIdE $ jsArgs arg) fStmts

compileContinuation :: forall a b . (SunroofArgument a, Sunroof b)
                => (a -> JS B b)
                -> CompM Expr
compileContinuation m2 = do
  (arg :: a) <- jsValue
  fStmts <- compile $ extractProgramJS (\ _ -> JS $ \ k -> k ()) (m2 arg)
  return $ Function (map varIdE $ jsArgs arg) fStmts


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
  return ([ mkVarStmt c $ case e' of
                          Inst expr -> fmap (ExprE . findExpr jsVars dbF) expr
                          Copy n'   -> -- Apply (ExprE (Var "COPY")) [ ExprE $ findExpr jsVars dbF n' ]
                                       findExpr jsVars dbF n'
          | n <- ids
          , Just c    <- return $ Map.lookup n jsVars
          , Just e' <- return $ Map.lookup n dbF
          ], findExpr jsVars dbF start)


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

mkVarStmt :: Id -> Expr -> Stmt
mkVarStmt v e = AssignStmt (VarRhs v) e
