module Test.Core where

import Control.Exception (throwIO)
import Control.Monad.Error (runErrorT)
import Control.Monad.State (evalStateT)
import Data.IORef (newIORef)
import Data.Map (empty)

import Core
import Initial
import Types.Core
import Types.Syntax.After

testScheme :: String -> IO [Expr]
testScheme str = do
    ref <- newIORef initialEnv
    result <- evalStateT (runErrorT $ runSchemeT $ scheme ref str) empty
    either throwIO return result
