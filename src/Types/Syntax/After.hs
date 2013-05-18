module Types.Syntax.After where

import Data.IORef.Lifted (IORef)
import Data.Map (Map)

import Types.Util

data Expr
    = Const Const
    | Var Ident
    | Define Ident Expr
    | Lambda Args Expr
    | Func Args Expr EnvRef
    | Apply Expr [Expr]
    | Prim Prim
    | Quote Expr
    | Begin [Expr]
    | Set Ident Expr
    | If Expr Expr Expr
    | Undefined

type CC = Expr

instance Show Expr where
    show (Const c) = show c
    show (Var v) = v
    show (Define v e) = "(define " ++ v ++ " " ++ show e ++ ")"
    show (Lambda args e) = "(lambda " ++ show args ++ " " ++ show e ++ ")"
    show (Func args e _) = "(lambda " ++ show args ++ " " ++ show e ++ ")"
    show (Apply e es) = "(" ++ unwords (map show $ e:es) ++ ")"
    show (Prim p) = show p
    show (Quote e) = "'" ++ show e
    show (Begin es) = "(begin " ++ unwords (map show es) ++ ")"
    show (Set v e) = "(set! " ++ v ++ " " ++ show e ++ ")"
    show (If b t f) = "(if " ++ show b ++ " " ++ show t ++ " " ++ show f ++ ")"
    show Undefined = "<#undef>"

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

instance Show Prim where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Equal = "="

newtype Args = Args (List Ident)

instance Show Args where
    show (Args (ProperList xs)) = "(" ++ unwords xs ++ ")"
    show (Args (DottedList xs x)) = "(" ++ unwords xs ++ " . " ++ x ++ ")"