{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Main where

import Control.Exception.Lifted (try)
#ifdef DEBUG
import Control.Monad.IO.Class (MonadIO (..))
#else
import Control.Monad ((>=>))
#endif
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import System.IO (hFlush, stdout)

import Conversion.Normalize (normalize)
import Conversion.CPS (cps)
import Eval (eval)
import Initial (initialEnv)
import Parser (parse)
import Types.Exception (SchemeException (..))
import Types.Syntax.After (Expr, EnvRef)

main :: IO ()
main = newIORef initialEnv >>= repl

#ifdef DEBUG
scheme :: (MonadBaseControl IO m, MonadIO m) => EnvRef -> String -> m Expr
scheme ref s = do
    be <- parse s
    liftIO $ putStrLn $ "parse: " ++ show be
    ae <- normalize be
    liftIO $ putStrLn $ "normalize: " ++ show ae
    let ce = cps ae
    liftIO $ putStrLn $ "cps: " ++ show ce
    eval ref ce
#else
scheme :: MonadBaseControl IO m => EnvRef -> String -> m Expr
scheme ref = parse >=> normalize >=> eval ref . cps
#endif

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
