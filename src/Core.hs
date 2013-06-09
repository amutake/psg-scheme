{-# LANGUAGE FlexibleContexts, CPP, ConstraintKinds #-}

module Core where

import Control.Exception (try)
#ifdef DEBUG
#else
import Control.Monad ((>=>))
#endif
import Control.Monad.Error (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)

import Env
import Primitives
import Conversion.CPS (cps)
import Conversion.Normalize (normalize)
import Parser (parse)
import Types.Core
import Types.Exception
import Types.Syntax
import Util

#ifdef DEBUG
scheme :: MonadScheme m => EnvRef -> String -> SchemeT m [Expr]
scheme ref s = do
    bes <- parse s
    liftIO $ putStrLn $ "parse: " ++ show bes
    mapM (\be -> do
        ae <- normalize be
        liftIO $ putStrLn $ "normalize: " ++ show ae
        let ce = cps ae
        liftIO $ putStrLn $ "cps: " ++ show ce
        eval ref ce
        ) bes
#else
scheme :: MonadScheme m => EnvRef -> String -> SchemeT m [Expr]
scheme ref = parse >=> mapM (normalize >=> eval ref . cps)
#endif

----------------
-- eval
----------------

eval :: MonadScheme m => EnvRef -> Expr -> SchemeT m Expr
eval _ c@(Const _) = return c
eval ref (Ident i) = lookupEnv ref i
eval ref (List (ProperList es)) = evalList ref es
eval _ (List (DottedList _ _)) = throwError $ SyntaxError "dotted list"
eval _ n@(Normalized _) = return n
eval _ e@(Evaled _) = return e

evalList :: MonadScheme m => EnvRef -> [Expr] -> SchemeT m Expr
evalList ref [Ident "define", Ident v, e] = eval ref e >>= define ref v
evalList ref [Ident "lambda", List args, body] = do
    args' <- extractIdents args
    return $ Evaled $ Func args' body ref
evalList _ [Ident "quote", e] = return e
evalList ref ((Ident "begin") : es) = mapM (eval ref) (init es) >> eval ref (last es)
evalList ref [Ident "set!", Ident v, e] = eval ref e >>= setVar ref v
evalList ref [Ident "if", b, t, f] = do
    b' <- eval ref b
    case b' of
        Const (Bool False) -> eval ref f
        _ -> eval ref t
evalList ref [Ident "load", e] = do
    e' <- eval ref e
    case e' of
        Const (String s) -> load ref s
        _ -> throwError $ TypeMismatch "String"
evalList ref (f : es) = do
    f' <- eval ref f
    es' <- mapM (eval ref) es
#ifdef DEBUG
    liftIO $ putStrLn $ ("  apply-before: " ++) $ show $ List $ ProperList (f : es)
    liftIO $ putStrLn $ ("  fun-args-eval: " ++) $ show $ List $ ProperList (f' : es')
    -- env <- readIORef ref
    -- liftIO $ putStrLn $ show env
#endif
    case f' of
        Evaled Return -> throwError $ Break $ last es'
        _ -> do
            e' <- apply ref f' es'
#ifdef DEBUG
            liftIO $ putStrLn $ ("  apply-result: " ++) $ show e'
            liftIO $ putStrLn ""
#endif
            return e'
evalList _ [] = return $ List nil

apply :: MonadScheme m => EnvRef -> Expr -> [Expr] -> SchemeT m Expr
apply _ (Normalized (Prim f)) es = applyPrim f es
apply _ (Evaled (Func args body closure)) es = do
    ref <- newIORef $ Extended empty closure
    defines ref args es
    eval ref body
apply _ e _ = throwError $ NotFunction $ show e

load :: MonadScheme m => EnvRef -> FilePath -> SchemeT m Expr
load ref path = do
    result <- liftIO $ try $ readFile path
    case result of
        Left err -> throwError $ IOError err
        Right str -> (scheme ref str >> return (Const $ Bool True)) `catchError`
            (\err -> liftIO (print err) >> return (Const $ Bool False))
