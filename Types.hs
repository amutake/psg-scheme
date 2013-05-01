module Types where

data Value
    = Bool Bool
    | Number Integer
    | String String
    | List [Value]
    deriving (Show)
