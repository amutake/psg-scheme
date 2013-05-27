{-# LANGUAGE FlexibleContexts #-}

module Initial where

import Control.Monad.Error (runErrorT)
import Control.Monad.State (execStateT)
import Data.Map (Map, fromList, empty)

import Core (scheme)
import Types.Core
import Types.Macro
import Types.Syntax.After
import Types.Util

initialEnv :: Env
initialEnv = Global primitives

primitives :: Map Ident Expr
primitives = fromList
    [
    ]

initialLoad :: EnvRef -> IO Macro
initialLoad ref = execStateT (runErrorT $ runSchemeT $ scheme ref loadStr) empty
  where
    loadStr = concat $ map (\s -> "(load \"" ++ s ++ "\")")
        [ "lib/util.scm"
        ]
