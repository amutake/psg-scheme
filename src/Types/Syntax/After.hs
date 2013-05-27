module Types.Syntax.After where

import Data.IORef.Lifted (IORef)
import Data.Map (Map)

import Types.Util

data Expr
    = Const Const
    | Var Ident
    | Define Ident Expr
    | DefineMacro Ident Expr
    | Lambda Args Expr
    | Func Args Expr EnvRef
    | Apply Expr [Expr]
    | Dot [Expr] Expr
    | CallCC CC Args Expr
    | Prim Prim
    | Quote Expr
    | QuasiQuote Expr
    | Begin [Expr]
    | Set Ident Expr
    | If Expr Expr Expr
    | Load Expr
    | Undefined
    | End
    deriving (Eq)

type CC = Expr

instance Show Expr where
    show (Const c) = show c
    show (Var v) = v
    show (Define v e) = "(define " ++ v ++ " " ++ show e ++ ")"
    show (DefineMacro v e) = "(define-macro " ++ v ++ " " ++ show e ++ ")"
    show (Lambda args e) = "(lambda " ++ show args ++ " " ++ show e ++ ")"
    show (Func args e _) = "(function " ++ show args ++ " " ++ show e ++ ")"
    show (Apply e es) = "(" ++ unwords (map show $ e:es) ++ ")"
    show (Dot es e) = "(" ++ unwords (map show es) ++ " . " ++ show e ++ ")"
    show (CallCC cc args body) = "((lambda " ++ show args ++ " " ++ show body ++ ") " ++ show cc ++ ")"
    show (Prim p) = show p
    show (Quote e) = "'" ++ show e
    show (QuasiQuote e) = "`" ++ show e
    show (Begin es) = "(begin " ++ unwords (map show es) ++ ")"
    show (Set v e) = "(set! " ++ v ++ " " ++ show e ++ ")"
    show (If b t f) = "(if " ++ show b ++ " " ++ show t ++ " " ++ show f ++ ")"
    show (Load e) = "(load " ++ show e ++ ")"
    show Undefined = "#undef"
    show End = "#end"

data Env
    = Global (Map Ident Expr)
    | Extended (Map Ident Expr) EnvRef

type EnvRef = IORef Env

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

prim :: Ident -> Expr
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
