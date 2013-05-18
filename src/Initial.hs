{-# LANGUAGE FlexibleContexts #-}

module Initial where

import Data.Map (Map, fromList)

import Types.Env
import Types.Syntax.After
import Types.Util

initialEnv :: Env
initialEnv = Global primitives

primitives :: Map Ident Expr
primitives = fromList
    [
    ]
