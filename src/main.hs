{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Main where

import Control.Monad.Error (runErrorT)
import Control.Monad.State (runStateT)
import Data.IORef (newIORef)
import Data.List (intersperse)
import System.IO (hFlush, stdout)

import Core (scheme)
import Initial (initialEnv, initialLoad)
import Types.Core (SchemeT (..))
import Types.Exception (SchemeException (..))
import Types.Syntax (EnvRef, Macro)

main :: IO ()
main = do
    ref <- newIORef initialEnv
    mac <- initialLoad ref
    repl mac ref

repl :: Macro -> EnvRef -> IO ()
repl mac ref = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    (results, mac') <- runStateT (runErrorT $ runSchemeT $ scheme ref str) mac
    case results of
        Left Exit -> putStrLn "Bye."
        Left (err :: SchemeException) -> next (show err) mac' ref
        Right es -> next (newline es) mac' ref
  where
    next e m r = putStrLn e >> repl m r
    newline = concat . intersperse "\n" .  map show
