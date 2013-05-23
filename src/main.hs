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
scheme :: (Functor m, Monad m, MonadIO m, MonadBase IO m) => EnvRef -> String -> SchemeT m [Expr]
scheme ref s = do
    bes <- parse s
    liftIO $ putStrLn $ "parse: " ++ show bes
    mapM (\be -> do
        ae <- normalize be
        liftIO $ putStrLn $ "normalize: " ++ show ae
        me <- macro ae
        liftIO $ putStrLn $ "macro: " ++ show me
        let ce = cps me
        liftIO $ putStrLn $ "cps: " ++ show ce
        eval ref ce
        ) bes
#else
scheme :: (Functor m, Monad m, MonadBase IO m, MonadIO m) => EnvRef -> String -> SchemeT m [Expr]
scheme ref = parse >=> mapM (normalize >=> macro >=> eval ref . cps)
#endif

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
