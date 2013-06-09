{-# LANGUAGE FlexibleContexts, CPP, ConstraintKinds, ScopedTypeVariables #-}

module Core where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (try)
#ifdef DEBUG
#else
import Control.Monad ((>=>))
#endif
import Control.Monad.Error (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (..))
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)
import qualified Data.Map as M

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
        me <- macro be
        liftIO $ putStrLn $ "macro: " ++ show me
        ae <- normalize me
        liftIO $ putStrLn $ "normalize: " ++ show ae
        let ce = cps ae
        liftIO $ putStrLn $ "cps: " ++ show ce
        catchError (eval ref ce) catchBreak
        ) bes
#else
scheme :: MonadScheme m => EnvRef -> String -> SchemeT m [Expr]
scheme ref = parse >=> mapM (macro >=> normalize >=> flip catchError catchBreak . eval ref . cps)
#endif

catchBreak :: MonadScheme m => SchemeException -> SchemeT m Expr
catchBreak (Break e) = return e
catchBreak e = throwError e

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
    liftIO $ putStrLn $ show $ f' : es'
#endif
    case f' of
        Evaled Return -> throwError $ Break $ last es'
        _ -> apply ref f' es'
evalList _ [] = return $ List nil

apply :: MonadScheme m => EnvRef -> Expr -> [Expr] -> SchemeT m Expr
apply ref (Normalized (Prim f)) (cc:es) =
    applyPrim f es >>= eval ref . List . ProperList . two cc . List . ProperList . two (Ident "quote")
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
      return $ Ident var
putMacro _ _ = throwError $ SyntaxError "define-macro"

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
macroList ((Ident v) : es) = do
      m <- get
      maybe (List . ProperList <$> (Ident v :) <$> mapM macro es) (conv es) $ M.lookup v m
macroList es = List . ProperList <$> mapM macro es

conv :: MonadScheme m => [Expr] -> MacroBody -> SchemeT m Expr
conv args (MacroBody args' body ref) = do
    ref' <- newIORef $ Extended empty ref
    defines ref' args' $ idE ref' "##macro-cont" : args
    e <- eval ref' body
    macro e
