{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Main where

#ifdef DEBUG
#else
import Control.Monad ((>=>))
#endif
import Control.Monad.Base (MonadBase)
import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (runStateT)
import Data.IORef (newIORef)
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
scheme :: (Functor m, Monad m, MonadIO m, MonadBase IO m) => EnvRef -> String -> SchemeT m Expr
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
scheme :: (Functor m, Monad m, MonadBase IO m, MonadIO m) => EnvRef -> String -> SchemeT m Expr
scheme ref = parse >=> normalize >=> macro >=> eval ref . cps
#endif

repl :: Macro -> EnvRef -> IO ()
repl mac ref = do
    putStr "scheme> "
    hFlush stdout
    str <- getLine
    (result, mac') <- runStateT (runErrorT $ runSchemeT $ scheme ref str) mac
    case result of
        Left Exit -> putStrLn "Bye."
        Left (Next e) -> print e >> repl mac' ref
        Left (err :: SchemeException) -> do
            print err
            repl mac' ref
        Right val -> do
            liftIO $ print val
            repl mac' ref
