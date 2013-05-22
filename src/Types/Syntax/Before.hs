module Types.Syntax.Before where

import Types.Util

data Expr
    = Const Const
    | Ident Ident
    | List (List Expr)
    deriving (Eq)

instance Show Expr where
    show (Const c) = show c
    show (Ident i) = i
    show (List l) = show l
