module Types.Syntax where

import Control.Applicative ((<$>), (<*>))
import Data.Foldable (Foldable (..))
import Data.IORef.Lifted (IORef)
import Data.Map (Map)
import Data.Traversable (Traversable (..))
import Prelude hiding (foldr)

data Expr
    = Const Const
    | Ident Ident
    | List (List Expr)
    | Normalized Normalized
    | Evaled Evaled
    deriving (Eq)

instance Show Expr where
    show (Const c) = show c
    show (Ident i) = show i
    show (List l) = show l
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
    | Eqv
    | Car
    | Cdr
    | Cons
    deriving (Eq)

instance Show Prim where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Equal = "="
    show Eqv = "eqv?"
    show Car = "car"
    show Cdr = "cdr"
    show Cons = "cons"

prim :: Ident -> Normalized
prim "+" = Prim Add
prim "-" = Prim Sub
prim "*" = Prim Mul
prim "/" = Prim Div
prim "=" = Prim Equal
prim "eqv?" = Prim Eqv
prim "car" = Prim Car
prim "cdr" = Prim Cdr
prim "cons" = Prim Cons
prim v = Var v

data Evaled
    = Func Args Expr EnvRef
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

type Macro = Map Ident MacroBody

data MacroBody = MacroBody Args Expr EnvRef

type CC = Expr
