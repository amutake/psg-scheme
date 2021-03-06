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
    | Prim Prim
    | Func (List Ident) Expr EnvRef
    | Return
    deriving (Eq)

instance Show Expr where
    show (Const c) = show c
    show (Ident i) = i
    show (List l) = show l
    show (Prim p) = show p
    show (Func args e _) = "(function " ++ show (Args args) ++ " " ++ show e ++ ")"
    show Return = "#return"

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
    | EqualP
    | EqP
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
    show EqualP = "equal?"
    show EqP = "eq?"

prim :: Ident -> Expr
prim "+" = Prim Add
prim "-" = Prim Sub
prim "*" = Prim Mul
prim "/" = Prim Div
prim "=" = Prim Equal
prim "<" = Prim NLT
prim ">" = Prim NGT
prim "eqv?" = Prim Eqv
prim "car" = Prim Car
prim "cdr" = Prim Cdr
prim "cons" = Prim Cons
prim "pair?" = Prim Pair
prim "number?" = Prim NumberP
prim "symbol?" = Prim SymbolP
prim "boolean?" = Prim BooleanP
prim "string?" = Prim StringP
prim "string-append" = Prim StringAppend
prim "symbol->string" = Prim SymbolString
prim "string->symbol" = Prim StringSymbol
prim "number->string" = Prim NumberString
prim "string->number" = Prim StringNumber
prim "procedure?" = Prim ProcP
prim "equal?" = Prim EqualP
prim "eq?" = Prim EqP
prim v = Ident v

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
