{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Main where

import Control.Monad.Error (runErrorT)
import Control.Monad.State (runStateT)
import Data.IORef (newIORef)
import Data.Map (empty)
import System.IO (hFlush, stdout)

import Core (scheme)
import Initial (initialEnv)
import Types.Core (SchemeT (..))
import Types.Exception (SchemeException (..))
import Types.Macro (Macro)
import Types.Syntax.After (EnvRef)

main :: IO ()
main = newIORef initialEnv >>= repl empty

repl :: Macro -> EnvRef -> IO ()
repl mac ref = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    (results, mac') <- runStateT (runErrorT $ runSchemeT $ scheme ref str) mac
    case results of
        Left Exit -> putStrLn "Bye."
        Left (err :: SchemeException) -> next err mac' ref
        Right es -> next es mac' ref
  where
    next e m r = print e >> repl m r
