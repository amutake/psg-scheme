module Types where

data Value
    = Bool Bool
    | Number Integer
    | String String
    | List [Value]
    | Ident Ident
    deriving (Show, Eq)

type Ident = String
