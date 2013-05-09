{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Main where

import Control.Exception.Lifted (try)
import Control.Monad ((>=>))
import Control.Monad.Base (MonadBase)
import Data.IORef
import Data.Map (empty)
import System.IO (hFlush, stdout)

import Eval (eval)
import Parser (parse)
import Types

main :: IO ()
main = newIORef (Global empty) >>= repl

scheme :: MonadBase IO m => String -> SchemeT m Value
scheme = parse >=> eval

repl :: IORef Env -> IO ()
repl env = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    result <- runSchemeT (try $ scheme str)
    case result of
        Left Exit -> putStrLn "bye."
        Left (err :: SchemeException) -> do
            print err
            repl env
        Right val -> do
            print val
            repl env
