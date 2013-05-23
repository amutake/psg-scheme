{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Main where

import Control.Exception.Lifted (try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad ((>=>))
import Control.Monad.State (runStateT, StateT, evalStateT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import Data.Map (empty)
import System.IO (hFlush, stdout)

import Conversion.Macro (macro)
import Conversion.Normalize (normalize)
import Conversion.CPS (cps)
import Eval (eval)
import Initial (initialEnv)
import Parser (parse)
import Types.Core (SchemeT (..))
import Types.Exception (SchemeException (..))
import Types.Macro (Macro)
import Types.Syntax.After (Expr, EnvRef)

main :: IO ()
main = do
    ref <- newIORef initialEnv
    evalStateT (repl ref) empty
    return ()

#ifdef DEBUG
scheme :: (MonadBaseControl IO m, MonadIO m) => EnvRef -> String -> SchemeT m Expr
scheme ref s = do
    be <- parse s
    liftIO $ putStrLn $ "parse: " ++ show be
    ae <- normalize be
    liftIO $ putStrLn $ "normalize: " ++ show ae
    me <- macro ae
    liftIO $ putStrLn $ "macro: " ++ show me
    let ce = cps ae
    liftIO $ putStrLn $ "cps: " ++ show ce
    eval ref ce
#else
scheme :: MonadBaseControl IO m => EnvRef -> String -> SchemeT m Expr
scheme ref = parse >=> normalize >=> macro >=> eval ref . cps
#endif

repl :: EnvRef -> StateT Macro IO ()
repl env = do
    liftIO $ putStr "scheme> "
    liftIO $ hFlush stdout
    str <- liftIO getLine
    result <- runSchemeT (try $ scheme env str)
    liftIO $ print result
    case result of
        Left Exit -> liftIO $ putStrLn "bye."
        Left (err :: SchemeException) -> do
            liftIO $ print err
            repl env
        Right val -> do
            liftIO $ print val
            repl env
