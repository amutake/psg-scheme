{-# LANGUAGE FlexibleContexts, CPP, ConstraintKinds #-}

module Core where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (try)
#ifdef DEBUG
#else
import Control.Monad ((>=>))
#endif
import Control.Monad.Base (MonadBase)
import Control.Monad.Error (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (..))
#ifdef DEBUG
import Data.IORef.Lifted (newIORef, readIORef)
#else
import Data.IORef.Lifted (newIORef)
#endif
import Data.Map (empty)
import qualified Data.Map as M

import Env
import Primitives
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
        me <- macro be
        liftIO $ putStrLn $ "macro: " ++ show me
        ae <- normalize me
        liftIO $ putStrLn $ "normalize: " ++ show ae
        eval ref ae
        ) bes
#else
scheme :: MonadScheme m => EnvRef -> String -> SchemeT m [Expr]
scheme ref = parse >=> mapM (macro >=> normalize >=> eval ref)
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
evalList ref [Ident "define-macro", Ident v, e] = eval ref e >>= putMacro v
evalList ref [Ident "lambda", List args, body] = do
    args' <- extractIdents args
    return $ Evaled $ Func args' body ref
evalList _ [Ident "quote", e] = return e
evalList ref [Ident "quasiquote", e] = evalQuasiQuote ref e
evalList ref [Ident "unquote", e] = eval ref e
evalList ref [Ident "unquote-splicing", e] = eval ref e
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
        Evaled Return -> return $ last es'
        _ -> do
            e' <- apply ref f' es'
#ifdef DEBUG
            liftIO $ putStrLn $ ("  apply-result: " ++) $ show e'
            liftIO $ putStrLn ""
#endif
            return e'
evalList _ [] = return $ List nil

evalQuasiQuote :: MonadScheme m => EnvRef -> Expr -> SchemeT m Expr
evalQuasiQuote _ c@(Const _) = return c
evalQuasiQuote _ i@(Ident _) = return i
evalQuasiQuote ref (List (ProperList es)) = evalListQuasiQuote ref es
evalQuasiQuote _ d@(List (DottedList _ _)) = return d
evalQuasiQuote _ n@(Normalized _) = return n
evalQuasiQuote _ e@(Evaled _) = return e

evalListQuasiQuote :: MonadScheme m => EnvRef -> [Expr] -> SchemeT m Expr
evalListQuasiQuote _ [Ident "quote", e] = return $ List $ ProperList [Ident "quote", e]
evalListQuasiQuote ref [Ident "unquote", e] = eval ref e
evalListQuasiQuote _ [] = return $ List nil
evalListQuasiQuote ref es = List . ProperList <$> go ref es
  where
    go _ [] = return []
    go ref' ((List (ProperList [Ident "unquote-splicing", e])) : es') = do
        e' <- eval ref' e
#ifdef DEBUG
        -- env <- readIORef ref'
        -- liftIO $ putStrLn $ show env
        -- liftIO $ putStrLn $ ("  evalQuasiQuote: " ++) $ show $ List $ ProperList (e : es')
#endif
        case e' of
            List (ProperList es'') -> (es'' ++) <$> go ref' es'
            _ -> throwError $ SyntaxError ",@expr must be evaluated to list"
    go ref' (e : es') = (:) <$> evalQuasiQuote ref' e <*> go ref' es'

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

putMacro :: MonadScheme m => Ident -> Expr -> SchemeT m Expr
putMacro var (Evaled (Func args expr ref)) = do
    mac <- get
    put $ M.insert var (MacroBody args expr ref) mac
#ifdef DEBUG
    -- mac' <- get
    -- liftIO $ print mac'
#endif
    return $ Ident var
putMacro _ _ = throwError $ SyntaxError "define-macro"

----------------
-- macro-expansion
----------------

macro :: MonadScheme m => Expr -> SchemeT m Expr
macro c@(Const _) = return c
macro i@(Ident _) = return i
macro (List (ProperList es)) = macroList es
macro (List (DottedList es e)) = List <$> (DottedList <$> mapM macro es <*> macro e)
macro n@(Normalized _) = return n
macro e@(Evaled _) = return e

macroList :: MonadScheme m => [Expr] -> SchemeT m Expr
macroList [] = return $ List nil
macroList ((Ident "quote") : es) = return $ List . ProperList $ Ident "quote" : es
macroList ((Ident "quasiquote") : es) = return $ List . ProperList $ Ident "quasiquote" : es
macroList ((Ident v) : es) = do
    m <- get
    maybe (List . ProperList <$> (Ident v :) <$> mapM macro es) (conv es) $ M.lookup v m
macroList es = List . ProperList <$> mapM macro es

conv :: MonadScheme m => [Expr] -> MacroBody -> SchemeT m Expr
conv args (MacroBody args' body ref) = do
    ref' <- newIORef $ Extended empty ref
    defines ref' args' $ args
#ifdef DEBUG
    liftIO $ putStrLn $ "exprs: " ++ show args
    liftIO $ print $ MacroBody args' body ref
#endif
    e <- eval ref' body
    macro e
