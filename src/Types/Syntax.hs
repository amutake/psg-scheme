module Types.Syntax where

import Control.Applicative ((<$>), (<*>))
import Data.Foldable (Foldable (..))
import Data.IORef.Lifted (IORef, readIORef)
import Data.Map (Map, toList)
import Data.Traversable (Traversable (..))
import Prelude hiding (foldr)
import System.IO.Unsafe (unsafePerformIO)

data Expr
    = Const Const
    | Ident Ident
    | List (List Expr)
    | Normalized Normalized
    | Evaled Evaled
    deriving (Eq)

instance Show Expr where
    show (Const c) = show c
    show (Ident i) = i
    show (List l) = show l
    show (Normalized n) = show n
    show (Evaled e) = show e

type Ident = String

data Const
    = Bool Bool
    | Number Integer
    | String String
    | Undefined
    deriving (Eq)

instance Show Const where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (String s) = show s
    show Undefined = "#undef"

data List a
    = ProperList [a]
    | DottedList [a] a
    deriving (Eq)

instance Show a => Show (List a) where
    show (ProperList xs) = "(" ++ unwords (map show xs) ++ ")"
    show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"

instance Functor List where
    fmap f (ProperList xs) = ProperList (fmap f xs)
    fmap f (DottedList xs x) = DottedList (fmap f xs) (f x)

instance Foldable List where
    foldr f z (ProperList xs) = foldr f z xs
    foldr f z (DottedList xs x) = foldr f z (xs ++ [x])

instance Traversable List where
    traverse f (ProperList xs) = ProperList <$> traverse f xs
    traverse f (DottedList xs x) = DottedList <$> traverse f xs <*> f x

data Normalized = Prim Prim deriving (Eq)

instance Show Normalized where
    show (Prim p) = show p

data Prim
    = Add
    | Sub
    | Mul
    | Div
    | Equal
    | NLT
    | NGT
    | Eqv
    | Car
    | Cdr
    | Cons
    | Pair
    | NumberP
    | SymbolP
    | BooleanP
    | StringP
    | StringAppend
    | SymbolString
    | StringSymbol
    | NumberString
    | StringNumber
    | ProcP
    deriving (Eq)

instance Show Prim where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Equal = "="
    show NLT = "<"
    show NGT = ">"
    show Eqv = "eqv?"
    show Car = "car"
    show Cdr = "cdr"
    show Cons = "cons"
    show Pair = "pair?"
    show NumberP = "number?"
    show SymbolP = "symbol?"
    show BooleanP = "boolean?"
    show StringP = "string?"
    show StringAppend = "string-append"
    show SymbolString = "symbol->string"
    show StringSymbol = "string->symbol"
    show NumberString = "number->string"
    show StringNumber = "string->number"
    show ProcP = "procedure?"

prim :: Ident -> Expr
prim "+" = Normalized $ Prim Add
prim "-" = Normalized $ Prim Sub
prim "*" = Normalized $ Prim Mul
prim "/" = Normalized $ Prim Div
prim "=" = Normalized $ Prim Equal
prim "<" = Normalized $ Prim NLT
prim ">" = Normalized $ Prim NGT
prim "eqv?" = Normalized $ Prim Eqv
prim "car" = Normalized $ Prim Car
prim "cdr" = Normalized $ Prim Cdr
prim "cons" = Normalized $ Prim Cons
prim "pair?" = Normalized $ Prim Pair
prim "number?" = Normalized $ Prim NumberP
prim "symbol?" = Normalized $ Prim SymbolP
prim "boolean?" = Normalized $ Prim BooleanP
prim "string?" = Normalized $ Prim StringP
prim "string-append" = Normalized $ Prim StringAppend
prim "symbol->string" = Normalized $ Prim SymbolString
prim "string->symbol" = Normalized $ Prim StringSymbol
prim "number->string" = Normalized $ Prim NumberString
prim "string->number" = Normalized $ Prim StringNumber
prim "procedure?" = Normalized $ Prim ProcP
prim v = Ident v

data Evaled
    = Func (List Ident) Expr EnvRef
    | Return
    deriving (Eq)

instance Show Evaled where
    show (Func args expr _) = "(function " ++ show args ++ " " ++ show expr ++ ")"
    show Return = "#return"

newtype Args = Args (List Ident) deriving (Eq)

instance Show Args where
    show (Args (ProperList xs)) = "(" ++ unwords xs ++ ")"
    show (Args (DottedList xs x)) = "(" ++ unwords xs ++ " . " ++ x ++ ")"

type EnvRef = IORef Env

data Env
    = Global (Map Ident Expr)
    | Extended (Map Ident Expr) EnvRef

instance Show Env where
    show (Global m) = concatMap (\(i, e) -> " - " ++ i ++ " : " ++ show e ++ "\n") $ toList m
    show (Extended m ref) = concatMap (\(i, e) -> " - " ++ i ++ " : " ++ show e ++ "\n") (toList m) ++
                            show (unsafePerformIO (readIORef ref))

type Macro = Map Ident MacroBody

data MacroBody = MacroBody (List Ident) Expr EnvRef

instance Show MacroBody where
    show (MacroBody args e ref) = "args: " ++ show args ++ ", body: " ++ show e ++ ",\nenv: " ++ envString
      where
        envString = unsafePerformIO $ do
            env <- readIORef ref
            return $ show env

type CC = Expr
