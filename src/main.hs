{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Main where

import Control.Exception.Lifted (try)
#ifdef DEBUG
import Control.Monad.IO.Class (MonadIO (..))
#else
import Control.Monad ((>=>))
#endif
import Control.Monad.State (runStateT)
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
main = newIORef initialEnv >>= repl empty

#ifdef DEBUG
scheme :: (MonadBaseControl IO m, MonadIO m) => EnvRef -> String -> SchemeT m Expr
scheme ref s = do
    be <- parse s
    liftIO $ putStrLn $ "parse: " ++ show be
    me <- macro be
    liftIO $ putStrLn $ "macro: " ++ show me
    ae <- normalize me
    liftIO $ putStrLn $ "normalize: " ++ show ae
    let ce = cps ae
    liftIO $ putStrLn $ "cps: " ++ show ce
    eval ref ce
#else
scheme :: MonadBaseControl IO m => EnvRef -> String -> SchemeT m Expr
scheme ref = parse >=> macro >=> normalize >=> eval ref . cps
#endif

repl :: Macro -> EnvRef -> IO ()
repl mac env = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    (result, mac') <- runStateT (runSchemeT (try $ scheme env str)) mac
    case result of
        Left Exit -> putStrLn "bye."
        Left (err :: SchemeException) -> do
            print err
            repl mac' env
        Right val -> do
            print val
            repl mac' env
