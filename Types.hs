module Types where

data Value
    = Bool Bool
    | Number Integer
    | String String
    | List [Value]
    | Ident Ident
    deriving (Show)

type Ident = String
