module Types where

data Value
    = Bool Bool
    | Number Integer
    | String String
    | Nil
    | Pair Value Value
    | Ident Ident
    deriving (Show, Eq)

type Ident = String
