{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Lifted (try)
import Control.Monad ((>=>))
import Control.Monad.Trans.Resource (MonadThrow (..))
import Data.Map (empty)
import System.IO (hFlush, stdout)

import Eval (eval)
import Parser (parse)
import Types

main :: IO ()
main = repl $ Global empty

scheme :: MonadThrow m => String -> SchemeT m Value
scheme = parse >=> eval

repl :: Env -> IO ()
repl env = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    (result, env') <- runScheme (try $ scheme str) env
    case result of
        Left Exit -> putStrLn "bye."
        Left (err :: SchemeException) -> do
            print err
            repl env
        Right val -> do
            print val
            repl env'
