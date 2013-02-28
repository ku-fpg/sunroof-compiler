{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Sunroof.Types where

import Prelude hiding (div, mod, quot, rem, floor, ceiling, isNaN, isInfinite)
import GHC.Exts
--import Data.Char ( isDigit, isControl, isAscii, ord )
import Data.List ( intercalate )
--import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Semigroup as Semi
import Control.Monad.Operational
import Data.Boolean
import Data.Boolean.Numbers
import Control.Monad
import Data.Char
import Data.AdditiveGroup
import Data.VectorSpace hiding ((<.>))
import Numeric ( showHex )
import Data.Proxy
import Data.Reify
import Control.Applicative ( Applicative, pure, (<$>))
import Data.Traversable
import Data.Foldable hiding (all, any)

type Uniq = Int         -- used as a unique label

cast :: (Sunroof a, Sunroof b) => a -> b
cast = box . unbox

-- cast to int?
int :: (Sunroof a) => a -> JSNumber
int = box . (\ e -> Op "(int)" [ExprE e]) . unbox

---------------------------------------------------------------
-- Trivial expression language for Java

type Id = String

type Expr = E ExprE

data ExprE = ExprE Expr
        deriving Show

data E expr
        = Lit String    -- a precompiled (atomic) version of this literal
        | Var Id
        | Op String [expr]
--        | BinOp String expr expr        -- We need to remove BinOp; this is a pretty print issues only
        | Function [Id] [Stmt]
        deriving Show

instance MuRef ExprE where
  type DeRef ExprE = E
  mapDeRef f (ExprE e) = traverse f e


instance Traversable E where
  traverse _ (Lit s) = pure (Lit s)
  traverse _ (Var s) = pure (Var s)
  traverse f (Op s xs) = Op s <$> traverse f xs
--  traverse f (BinOp s e1 e2) = BinOp s <$> f e1 <*> f e2
  traverse _ (Function nms stmts) = pure (Function nms stmts)

instance Foldable E where
  foldMap _ (Lit _) = mempty
  foldMap _ (Var _) = mempty
  foldMap f (Op _ xs) = foldMap f xs
--  foldMap f (BinOp s e1 e2) = foldMap f [e1,e2]
  foldMap _ (Function _nms _stmts) = mempty

instance Functor E where
  fmap _ (Lit s) = Lit s
  fmap _ (Var s) = Var s
  fmap f (Op s xs) = Op s (map f xs)
--  fmap f (BinOp s e1 e2) = BinOp s (f e1) (f e2)
  fmap _ (Function nms stmts) = Function nms stmts

--
--instance Show Expr where
--        show = showExpr False

showExpr :: Bool -> Expr -> String
showExpr _ (Lit a) = a  -- always stand alone, or pre-parenthesised
showExpr _ (Var v) = v  -- always stand alone
showExpr b e = p $ case e of
   (Op "[]" [ExprE a,ExprE x])   -> showExpr True a ++ "[" ++ showExpr False x ++ "]"
   (Op "?:" [ExprE a,ExprE x,ExprE y]) -> showExpr True a ++ "?" ++ showExpr True x ++ ":" ++ showExpr True y
   (Op op [ExprE x,ExprE y]) | not (any isAlpha op) -> showExpr True x ++ op ++ showExpr True y
   (Op fn args) -> fn ++ "(" ++ intercalate "," (map (\ (ExprE e') -> showExpr False e') args) ++ ")"
   (Function args body) ->
                "function" ++
                "(" ++ intercalate "," args ++ ") {\n" ++
                   indent 2 (unlines (map showStmt body)) ++
                "}"
 where
   p txt = if b then "(" ++ txt ++ ")" else txt

indent :: Int -> String -> String
indent n = unlines . map (take n (cycle "  ") ++) . lines

data Stmt
        = VarStmt Id Expr           -- var Id = Expr;   // Id is fresh
        | AssignStmt Expr Expr Expr -- Expr[Expr] = Expr
        | ExprStmt Expr             -- Expr
        | ReturnStmt Expr           -- return Expr
        | IfStmt Expr [Stmt] [Stmt] -- if (Expr) { Stmts } else { Stmts }
        | WhileStmt Expr [Stmt]     -- while (Expr) { Stmts }

instance Show Stmt where
        show = showStmt

showStmt :: Stmt -> String
showStmt (VarStmt v e) = "var " ++ v ++ " = " ++ showExpr False e ++ ";"
showStmt (AssignStmt e1 e2 e3) = showExpr True e1 ++ "[" ++ showExpr False e2 ++ "] = " ++ showExpr False e3 ++ ";"
showStmt (ExprStmt e) = showExpr False e ++ ";"
showStmt (ReturnStmt e) = "return " ++ showExpr False e ++ ";"
showStmt (IfStmt i t e) = "if(" ++ showExpr False i ++ "){\n"
  ++ indent 2 (unlines (map showStmt t))
  ++ "} else {\n"
  ++ indent 2 (unlines (map showStmt e))
  ++ "}"
showStmt (WhileStmt b stmts) = "while(" ++ showExpr False b ++ "){\n"
  ++ indent 2 (unlines (map showStmt stmts))
  ++ "}"

{-
        show (Lit a)  = a
        show (Var v) = v
        show (Op "[]" [a,x]) = "(" ++ show a ++ ")[" ++ show x ++ "]"
        show (Op "?:" [a,x,y]) = "((" ++ show a ++ ")?(" ++ show x ++ "):(" ++ show y ++ "))"
--        show (Op "(,)" [x,y]) = "[" ++ show x ++ "," ++ show y ++ "]"
        show (Op x [a,b]) | all (not . isAlpha) x = "(" ++ show a ++ ")" ++ x ++ "(" ++ show b ++ ")"
        show (Op fn args) = fn ++ "(" ++ intercalate "," (map show args) ++ ")"
--        show (Cast e) = show e
-}

---------------------------------------------------------------

litparen :: String -> String
litparen nm | all (\ c -> isDigit c || c == '.') nm = nm
            | otherwise      = "(" ++ nm ++ ")"

mkVar :: Sunroof a => Uniq -> a
mkVar = box . Var . ("v" ++) . show

class Show a => Sunroof a where
        box :: Expr -> a
        unbox :: a -> Expr

        showVar :: a -> String -- needed because show instance for unit is problematic
        showVar = show

        assignVar :: Proxy a -> Id -> Expr -> Stmt
        assignVar _ a rhs = VarStmt a rhs

-- unit is the oddball
instance Sunroof () where
        showVar _ = ""
        assignVar _ _ rhs = ExprStmt rhs
        box _ = ()
        unbox () = Lit ""
---------------------------------------------------------------

class Monad m => UniqM m where
        uniqM :: m Uniq

jsVar :: (Sunroof a, UniqM m) => m a
jsVar = uniqM >>= return . mkVar

---------------------------------------------------------------

class SunroofValue a where
  type ValueOf a :: *
  js :: a -> ValueOf a

instance SunroofValue () where
  type ValueOf () = ()
  js () = ()

---------------------------------------------------------------

class JSArgument args where
        jsArgs   :: args -> [Expr]        -- turn a value into a list of expressions
        jsValue  :: (UniqM m) => m args

instance JSArgument () where
        jsArgs _ = []
        jsValue = return ()

instance JSArgument JSBool where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance JSArgument JSNumber where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance JSArgument JSString where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance JSArgument JSObject where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance (Sunroof a, Sunroof b) => JSArgument (a,b) where
      jsArgs ~(a,b) = [unbox a, unbox b]
      jsValue = liftM2 (,) jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c) => JSArgument (a,b,c) where
      jsArgs ~(a,b,c) = [unbox a, unbox b, unbox c]
      jsValue = liftM3 (,,) jsVar jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d) => JSArgument (a,b,c,d) where
      jsArgs ~(a,b,c,d) = [unbox a, unbox b, unbox c, unbox d]
      jsValue = liftM4 (,,,) jsVar jsVar jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e) => JSArgument (a,b,c,d,e) where
      jsArgs ~(a,b,c,d,e) = [unbox a, unbox b, unbox c, unbox d, unbox e]
      jsValue = liftM5 (,,,,) jsVar jsVar jsVar jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f) => JSArgument (a,b,c,d,e,f) where
      jsArgs ~(a,b,c,d,e,f) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f]
      jsValue = return (,,,,,) `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar

-- Need to add the 7 & 8 tuple (not used in this package - yet)

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f, Sunroof g, Sunroof h, Sunroof i) => JSArgument (a,b,c,d,e,f,g,h,i) where
      jsArgs ~(a,b,c,d,e,f,g,h,i) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f, unbox g, unbox h, unbox i]
      jsValue = return (,,,,,,,,)
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar

----    jsValue :: val -> JSValue

--instance JSType JSBool where
--    jsValue (JSBool expr) = JSValue expr

--instance JSPrimitive a => JSArgument a where

---------------------------------------------------------------

data JSBool = JSBool Expr

instance Show JSBool where
        show (JSBool e) = showExpr False e

instance Sunroof JSBool where
        box = JSBool
        unbox (JSBool v)  = v

instance Boolean JSBool where
  true          = JSBool (Lit "true")
  false         = JSBool (Lit "false")
  notB  (JSBool e1) = JSBool $ Op "!" [ExprE e1]
  (&&*) (JSBool e1)
        (JSBool e2) = JSBool $ binOp "&&" e1 e2
  (||*) (JSBool e1)
        (JSBool e2) = JSBool $ binOp "||" e1 e2

type instance BooleanOf JSBool = JSBool

instance IfB JSBool where
    ifB = js_ifB

instance EqB JSBool where
  (==*) e1 e2 = JSBool $ binOp "==" (unbox e1) (unbox e2)
  (/=*) e1 e2 = JSBool $ binOp "!=" (unbox e1) (unbox e2)

js_ifB :: (Sunroof a) => JSBool -> a -> a -> a
js_ifB (JSBool c) t e = box $ Op "?:" [ExprE c,ExprE $ unbox t,ExprE $ unbox e]

instance SunroofValue Bool where
  type ValueOf Bool = JSBool
  js True = true
  js False = false

---------------------------------------------------------------

-- The first type argument is the type of function argument;
-- The second type argument of JSFunction is what the function returns.
data JSFunction args ret = JSFunction Expr

instance Show (JSFunction a r) where
        show (JSFunction v) = showExpr False v

instance forall a r . (JSArgument a, Sunroof r) => Sunroof (JSFunction a r) where
        box = JSFunction
        unbox (JSFunction e) = e
        assignVar _ a rhs = VarStmt a
                          $ Function args
                          [ ReturnStmt
                          $ Op (showExpr True rhs) (fmap (ExprE . Var) args) ]
          where args = [ 'a' : show (i :: Int)
                       | (i,_) <- zip [1..] (jsArgs (undefined :: a))]

type instance BooleanOf (JSFunction a r) = JSBool

instance (JSArgument a, Sunroof r) => IfB (JSFunction a r) where
    ifB = js_ifB

instance (JSArgument a, Sunroof b) => SunroofValue (a -> JS b) where
  type ValueOf (a -> JS b) = JS (JSFunction a b)
  js = function

---------------------------------------------------------------

binOp :: String -> Expr -> Expr -> E ExprE
binOp op e1 e2 = Op op [ExprE e1, ExprE e2]

uniOp :: String -> Expr -> E ExprE
uniOp op e = Op op [ExprE e]

---------------------------------------------------------------

data JSNumber = JSNumber Expr

instance Show JSNumber where
        show (JSNumber v) = showExpr False v

instance Sunroof JSNumber where
        box = JSNumber
        unbox (JSNumber e) = e

instance Num JSNumber where
        (JSNumber e1) + (JSNumber e2) = JSNumber $ binOp "+" e1 e2
        (JSNumber e1) - (JSNumber e2) = JSNumber $ binOp "-" e1 e2
        (JSNumber e1) * (JSNumber e2) = JSNumber $ binOp "*" e1 e2
        abs (JSNumber e1) = JSNumber $ uniOp "Math.abs" e1
        signum (JSNumber _e1) = error "signum" -- JSNumber $ uniOp "ERROR" e1
        fromInteger = JSNumber . Lit . litparen . show

instance IntegralB JSNumber where
  quot a b = ifB ((a / b) <* 0)
                 (JSNumber $ uniOp "Math.ceil" (let JSNumber res = a / b in res))
                 (a `div` b)
  rem a b = a - (a `quot` b)*b
  div a b = JSNumber $ uniOp "Math.floor" (let JSNumber res = a / b in res)
  mod (JSNumber a) (JSNumber b) = JSNumber $ binOp "%" a b


instance Fractional JSNumber where
        (JSNumber e1) / (JSNumber e2) = JSNumber $ binOp "/" e1 e2
        fromRational = JSNumber . Lit . litparen . show . (fromRational :: Rational -> Double)

instance Floating JSNumber where
        pi = JSNumber $ Lit $ "Math.PI"
        sin   (JSNumber e) = JSNumber $ uniOp "Math.sin"   e
        cos   (JSNumber e) = JSNumber $ uniOp "Math.cos"   e
        asin  (JSNumber e) = JSNumber $ uniOp "Math.asin"  e
        acos  (JSNumber e) = JSNumber $ uniOp "Math.acos"  e
        atan  (JSNumber e) = JSNumber $ uniOp "Math.atan"  e
        sinh  (JSNumber e) = JSNumber $ uniOp "Math.sinh"  e
        cosh  (JSNumber e) = JSNumber $ uniOp "Math.cosh"  e
        asinh (JSNumber e) = JSNumber $ uniOp "Math.asinh" e
        acosh (JSNumber e) = JSNumber $ uniOp "Math.acosh" e
        atanh (JSNumber e) = JSNumber $ uniOp "Math.atanh" e
        exp   (JSNumber e) = JSNumber $ uniOp "Math.exp"   e
        log   (JSNumber e) = JSNumber $ uniOp "Math.log"   e

instance RealFracB JSNumber where
  properFraction n =
    ( ifB (n >=* 0) (floor n) (ceiling n)
    , ifB (n >=* 0) (n - floor n) (n - ceiling n)
    )
  round   (JSNumber e) = JSNumber $ uniOp "Math.round" e
  ceiling (JSNumber e) = JSNumber $ uniOp "Math.ceil"  e
  floor   (JSNumber e) = JSNumber $ uniOp "Math.floor" e

instance RealFloatB JSNumber where
  isNaN (JSNumber a) = JSBool $ uniOp "isNaN" a
  isInfinite n = notB (isFinite n) &&* notB (isNaN n)
    where isFinite (JSNumber a) = JSBool $ uniOp "isFinite" a
  isNegativeZero n = isInfinite n &&* n <* 0
  isIEEE _ = true -- AFAIK
  atan2 (JSNumber a) (JSNumber b) = JSNumber $ Op "Math.atan2" [ExprE a, ExprE b]

type instance BooleanOf JSNumber = JSBool

instance IfB JSNumber where
  ifB = js_ifB

instance EqB JSNumber where
  (==*) e1 e2 = JSBool $ binOp "==" (unbox e1) (unbox e2)
  (/=*) e1 e2 = JSBool $ binOp "!=" (unbox e1) (unbox e2)

instance OrdB JSNumber where
  (>*)  e1 e2 = JSBool $ binOp ">"  (unbox e1) (unbox e2)
  (>=*) e1 e2 = JSBool $ binOp ">=" (unbox e1) (unbox e2)
  (<*)  e1 e2 = JSBool $ binOp "<"  (unbox e1) (unbox e2)
  (<=*) e1 e2 = JSBool $ binOp "<=" (unbox e1) (unbox e2)

instance AdditiveGroup JSNumber where
        zeroV = 0
        (^+^) = (+)
        negateV = negate

instance VectorSpace JSNumber where
  type Scalar JSNumber = JSNumber
  s *^ d = s * d

instance SunroofValue Double where
  type ValueOf Double = JSNumber
  js = box . Lit . litparen . show

instance SunroofValue Float where
  type ValueOf Float = JSNumber
  js = box . Lit . litparen . show

instance SunroofValue Int where
  type ValueOf Int = JSNumber
  js = fromInteger . toInteger

instance SunroofValue Integer where
  type ValueOf Integer = JSNumber
  js = fromInteger . toInteger

instance SunroofValue Rational where
  type ValueOf Rational = JSNumber
  js = box . Lit . litparen . (show :: Double -> String) . fromRational

-- -------------------------------------------------------------
-- Javascript Strings
-- -------------------------------------------------------------

data JSString = JSString Expr

instance Show JSString where
        show (JSString v) = showExpr False v

instance Sunroof JSString where
        box = JSString
        unbox (JSString e) = e

instance Semi.Semigroup JSString where
        (JSString e1) <>(JSString e2) = JSString $ binOp "+" e1 e2


instance Monoid JSString where
        mempty = fromString ""
        mappend (JSString e1) (JSString e2) = JSString $ binOp "+" e1 e2

instance IsString JSString where
    fromString = JSString . Lit . jsLiteralString

type instance BooleanOf JSString = JSBool

instance IfB JSString where
    ifB = js_ifB

instance EqB JSString where
    (==*) e1 e2 = JSBool $ binOp "==" (unbox e1) (unbox e2)
    (/=*) e1 e2 = JSBool $ binOp "!=" (unbox e1) (unbox e2)

instance SunroofValue [Char] where
  type ValueOf [Char] = JSString
  js = fromString

instance SunroofValue Char where
  type ValueOf Char = JSString
  js c = fromString [c]

-- -------------------------------------------------------------
-- String Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Transform a Haskell string into a string representing a JS string literal.
jsLiteralString :: String -> String
jsLiteralString = jsQuoteString . jsEscapeString

-- | Add quotes to a string.
jsQuoteString :: String -> String
jsQuoteString s = "\"" ++ s ++ "\""

-- | Transform a character to a string that represents its JS
--   unicode escape sequence.
jsUnicodeChar :: Char -> String
jsUnicodeChar c =
  let hex = showHex (ord c) ""
  in ('\\':'u': replicate (4 - length hex) '0') ++ hex

-- | Correctly replace Haskell characters by the JS escape sequences.
jsEscapeString :: String -> String
jsEscapeString [] = []
jsEscapeString (c:cs) = case c of
  -- Backslash has to remain backslash in JS.
  '\\' -> '\\' : '\\' : jsEscapeString cs
  -- Special control sequences.
  '\0' -> jsUnicodeChar '\0' ++ jsEscapeString cs -- Ambigous with numbers
  '\a' -> jsUnicodeChar '\a' ++ jsEscapeString cs -- Non JS
  '\b' -> '\\' : 'b' : jsEscapeString cs
  '\f' -> '\\' : 'f' : jsEscapeString cs
  '\n' -> '\\' : 'n' : jsEscapeString cs
  '\r' -> '\\' : 'r' : jsEscapeString cs
  '\t' -> '\\' : 't' : jsEscapeString cs
  '\v' -> '\\' : 'v' : jsEscapeString cs
  '\"' -> '\\' : '\"' : jsEscapeString cs
  '\'' -> '\\' : '\'' : jsEscapeString cs
  -- Non-control ASCII characters can remain as they are.
  c' | not (isControl c') && isAscii c' -> c' : jsEscapeString cs
  -- All other non ASCII signs are escaped to unicode.
  c' -> jsUnicodeChar c' ++ jsEscapeString cs

-- -------------------------------------------------------------
-- Javascript Objects
-- -------------------------------------------------------------

data JSObject = JSObject Expr

instance Show JSObject where
        show (JSObject v) = showExpr False v

instance Sunroof JSObject where
        box = JSObject
        unbox (JSObject o) = o

type instance BooleanOf JSObject = JSBool

instance IfB JSObject where
    ifB = js_ifB

instance SunroofValue Expr where
  type ValueOf Expr = JSObject
  js = box

-- -------------------------------------------------------------
-- Javascript Arrays
-- -------------------------------------------------------------

data JSArray a = JSArray Expr

instance Show (JSArray a) where
        show (JSArray v) = showExpr False v

instance (Sunroof a) => Sunroof (JSArray a) where
        box = JSArray
        unbox (JSArray o) = o

type instance BooleanOf (JSArray a) = JSBool

instance (Sunroof a) => IfB (JSArray a) where
    ifB = js_ifB
{- This conflicts with the string instance.
instance (SunroofValue a) => SunroofValue [a] where
  type ValueOf [a] = JSArray (ValueOf a)
  -- Uses JSON
  js l  = JSArray $ Lit $ "[" ++ intercalate "," (fmap (showVar . js) l) ++ "]"
-}

array :: (SunroofValue a, Sunroof (ValueOf a)) => [a] -> JSArray (ValueOf a)
array l  = JSArray $ Lit $ "[" ++ intercalate "," (fmap (showVar . js) l) ++ "]"

emptyArray :: (Sunroof a) => JSArray a
emptyArray = JSArray $ Lit "[]"

instance forall a . (Sunroof a) => Monoid (JSArray a) where
  mempty = emptyArray
  mappend (JSArray e1) (JSArray e2) = JSArray $ Op (concat e1) [ExprE e2]
    where
      concat :: Expr -> String
      concat e = showExpr True $ Op "[]" $
        [ExprE e, ExprE $ (unbox :: JSString -> Expr) $ fromString "concat"]

---------------------------------------------------------------

-- | a 'JSSelector' selects a field from a JSObject.
-- The phantom is the type of the selected value.
data JSSelector :: * -> * where
        JSSelector :: JSString           -> JSSelector a

instance IsString (JSSelector a) where
    fromString = JSSelector . fromString

instance Show (JSSelector a) where
    show (JSSelector str) = show str


label :: JSString -> JSSelector a
label = JSSelector

---------------------------------------------------------------

infixl 1 !

(!) :: forall a . (Sunroof a) => JSObject -> JSSelector a -> a
(!) arr (JSSelector idx) = box $ Op "[]" [ExprE $ unbox arr,ExprE $ unbox idx]

---------------------------------------------------------------

infix  5 :=


type Action a r = a -> JS r
{-
data Action :: * -> * -> * where
   -- Invoke is not quite right
   Invoke :: (Sunroof r) => [Expr]                              -> Action (JSFunction a r) r
   -- Basically, this is special form of call, to assign to a field
   (:=)   :: (Sunroof a) => JSSelector a -> a                   -> Action JSObject ()
   -- This is the fmap-like function, an effect-free modifier on the first argument
   -- TODO: revisit Map and Invoke. Does not feel right.
   Map :: (Sunroof b, a ~ JSObject, b ~ JSFunction x y, Sunroof c) => (a -> b) -> Action b c                 -> Action a c
   Field :: JSSelector a                                        -> Action JSObject a

   Dot :: Action a b -> Action b c                              -> Action a c

   NoAction :: b                                                -> Action a b
   BindAction :: Action a b -> (b -> Action a c)                -> Action a c

-- There is
--  Action JSObject a                           -- Return a             JSObject -> JS a
--  Action (JSFunction a r) r                   -- Function a r         JSFunction a r -> a -> JS r
-}
-- method :: String             -> a -> JSObject -> JS r
-- with :: a                    -> JSFunction a r -> JS r
-- (:=) :: JSSelector a -> a    -> JSObject -> JS ()

-- type Action a = JSObject -> JS a     ??? But we have the reader monad built in anyway???

-- (a -> JS b) -> (b -> JS c)
-- method :: String -> a -> Return r
-- with :: Return (JSFunction a b) -> a -> Return a
{-
instance Monad (Action a) where
        return = NoAction
        (>>=)  = BindAction
-}
---------------------------------------------------------------

-- TODO: not sure about the string => JSSelector (JSFunction a) overloading.
--method :: JSSelector (JSFunction a) -> [JSValue] -> Action JSObject a

-- SBC: call
method :: (JSArgument a, Sunroof r) => String -> a -> Action JSObject r
method str args obj = (obj ! attribute str) `apply` args

string :: String -> JSString
string = fromString

object :: String -> JSObject
object = JSObject . Lit

-- perhaps call this invoke, or fun
-- SBC: fun
call :: String -> JSFunction a r
call = JSFunction . Lit

with :: (JSArgument a, Sunroof r) => a -> Action (JSFunction a r) r
with a fn = JS $ singleton $ JS_Invoke (jsArgs a) fn

-- TODO: should take String argument
new :: JS JSObject
new = evaluate $ object "new Object()"

attribute :: String -> JSSelector a
attribute attr = label $ string attr

--vector :: [JSValue] -> JSVector
--vector = ...
---------------------------------------------------------------

--select :: (Sunroof a) => JSSelector a -> JSObject -> JS a
--select sel obj = evaluate (obj ! sel) JS $ singleton $ JS_Select sel obj

---------------------------------------------------------------

-- This is not the same as return; it evaluates
-- the argument to value form.
evaluate, var, value :: (Sunroof a) => a -> JS a
evaluate a  = JS $ singleton (JS_Eval a)

var = evaluate
value = evaluate

---------------------------------------------------------------

-- Control.Monad.Operational makes a monad out of JS for us
data JS a where
    JS   :: Program JSI a                                             -> JS a
    (:=) :: (Sunroof a) => JSSelector a -> a -> JSObject              -> JS ()

unJS :: JS a -> Program JSI a
unJS (JS m) = m
unJS ((:=) sel a obj) = singleton $ JS_Assign sel a obj

instance Monad JS where
        return a = JS (return a)
        m >>= k = JS (unJS m >>= \ r -> unJS (k r))

-- | We define the Semigroup instance for JS, where
--  the first result (but not the first effect) is discarded.
--  Thus, '<>' is the analog of the monadic '>>'.
instance Semi.Semigroup (JS a) where
        js1 <> js2 = js1 >> js2

instance Monoid (JS ()) where
        mempty = return ()
        mappend = (<>)

-- define primitive effects / "instructions" for the JS monad
data JSI a where

    -- apply an action to an 'a', and compute a b
--    JS_App    :: (Sunroof a, Sunroof b) => a -> Action a b      -> JSI b

    JS_Assign  :: (Sunroof a) => JSSelector a -> a -> JSObject  -> JSI ()

    JS_Select  :: (Sunroof a) => JSSelector a -> JSObject      -> JSI a

    JS_Invoke :: (JSArgument a, Sunroof r) => [Expr] -> JSFunction a r        -> JSI r        -- Perhaps take the overloaded vs [Expr]
                                                                                -- and use jsArgs in the compiler?

    -- Not the same as return; does evaluation of argument
    JS_Eval   :: (Sunroof a) => a                               -> JSI a

    JS_Function :: (JSArgument a, Sunroof b) => (a -> JS b)        -> JSI (JSFunction a b)
    -- Needs? Boolean bool, bool ~ BooleanOf (JS a)
    JS_Branch :: (Sunroof a, Sunroof bool) => bool -> JS a -> JS a -> JSI a
    -- A loop primitive.
    JS_Foreach :: (Sunroof a, Sunroof b) => JSArray a -> (a -> JS b) -> JSI ()

---------------------------------------------------------------

-- We only can compile functions that do not have interesting return
-- values, so we can assume they are continuation-like things.
function :: (JSArgument a, Sunroof b) => (a -> JS b) -> JS (JSFunction a b)
function = JS . singleton . JS_Function

infixl 1 `apply`

-- | Call a function with the given arguments.
apply :: (JSArgument args, Sunroof ret) => JSFunction args ret -> args -> JS ret
apply f args = f # with args

foreach :: (Sunroof a, Sunroof b) => JSArray a -> (a -> JS b) -> JS ()
foreach arr body = JS $ singleton $ JS_Foreach arr body

infixl 1 #

-- We should use this operator for the obj.label concept.
-- It has been used in other places (but I can not seems
-- to find a library for it)
(#) :: a -> (a -> JS b) -> JS b
(#) obj act = act obj

type instance BooleanOf (JS a) = JSBool

instance forall a . (Sunroof a) => IfB (JS a) where
    ifB i h e = JS $ singleton $ JS_Branch i h e

switch :: (EqB a, BooleanOf a ~ JSBool, Sunroof a, Sunroof b) => a -> [(a,JS b)] -> JS b
switch _a [] = return (cast (object "undefined"))
switch a ((c,t):e) = ifB (a ==* c) t (switch a e)

---------------------------------------------------------------
-- The JS (Blocking) is continuation based.
newtype JSB a = JSB { unJSB :: (a -> JS ()) -> JS () }

instance Monad JSB where
        return a = JSB $ \ k -> k a
        (JSB m) >>= k = JSB $ \ k0 -> m (\ a -> unJSB (k a) k0)

