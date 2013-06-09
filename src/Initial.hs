
{-# LANGUAGE FlexibleContexts #-}

module Initial where

import Control.Applicative ((<$>))
import Control.Monad.Error (runErrorT)
import Control.Monad.State (execStateT)
import Data.Map (Map, fromList, empty)

import Core (scheme)
import Types.Core
import Types.Syntax

initialEnv :: Env
initialEnv = Global primitives

primitives :: Map Ident Expr
primitives = fromList
    [
    ]

initialLoad :: EnvRef -> IO Macro
initialLoad ref = do
    last <$> mapM exec loadStr
  where
    exec str = execStateT (runErrorT $ runSchemeT $ scheme ref str) empty
    loadStr = map (\s -> "(load \"" ++ s ++ "\")")
        [ "lib/shiftreset.scm"
        , "lib/shift.scm"
        , "lib/reset.scm"
        , "lib/test.scm"
        ]
