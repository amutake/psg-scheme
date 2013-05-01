module Types where

data Value
    = Number Integer
    | String String
    | List [Value]
    deriving (Show)
