module Types.Env where

import Data.IORef.Lifted (IORef)
import Data.Map (Map)

import Types.Syntax.After
import Types.Util

data Env
    = Global (Map Ident Expr)
    | Extended (Map Ident Expr) EnvRef

type EnvRef = IORef Env
