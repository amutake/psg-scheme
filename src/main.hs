{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Main where

import Control.Exception.Lifted (try)
import Control.Monad ((>=>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import System.IO (hFlush, stdout)

import Eval (eval)
import Initial (initialEnv)
import Parser (parse)
import Types

main :: IO ()
main = newIORef initialEnv >>= repl

scheme :: MonadBaseControl IO m => EnvRef -> String -> SchemeT m Value
scheme env = parse >=> eval env

repl :: EnvRef -> IO ()
repl env = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    result <- runSchemeT (try $ scheme env str)
    case result of
        Left Exit -> putStrLn "bye."
        Left (err :: SchemeException) -> do
            print err
            repl env
        Right val -> do
            print val
            repl env
