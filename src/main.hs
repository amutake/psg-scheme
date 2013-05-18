{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Main where

import Control.Exception.Lifted (try)
import Control.Monad ((>=>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import System.IO (hFlush, stdout)

import Conversion.Normalize (normalize)
import Conversion.CPS (cps)
import Eval (eval)
import Initial (initialEnv)
import Parser (parse)
import Types.Env (EnvRef)
import Types.Exception (SchemeException (..))
import Types.Syntax.After (Expr)

main :: IO ()
main = newIORef initialEnv >>= repl

scheme :: MonadBaseControl IO m => EnvRef -> String -> m Expr
scheme ref = parse >=> normalize >=> eval ref . cps

repl :: EnvRef -> IO ()
repl env = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    result <- try $ scheme env str
    case result of
        Left Exit -> putStrLn "bye."
        Left (err :: SchemeException) -> do
            print err
            repl env
        Right val -> do
            print val
            repl env
