{-# LANGUAGE FlexibleContexts #-}

module Conversion.Normalize where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (throwError)
import Data.Traversable (Traversable, traverse)

import Types.Core
import Types.Exception
import qualified Types.Syntax.After as A
import qualified Types.Syntax.Before as B
import Types.Util

normalize :: (Functor m, Monad m) => B.Expr -> SchemeT m A.Expr
normalize expr = normalizeExpr $ flattenList expr

flattenList :: B.Expr -> B.Expr
flattenList (B.List (DottedList xs x)) = case x of
    B.List (ProperList ys) -> B.List $ ProperList $ map flattenList $ xs ++ ys
    B.List (DottedList ys y) -> B.List $ DottedList (map flattenList $ xs ++ ys) y
    y -> B.List $ DottedList xs y
flattenList v = v

normalizeExpr :: (Functor m, Monad m) => B.Expr -> SchemeT m A.Expr
normalizeExpr (B.Const c) = return $ A.Const c
normalizeExpr (B.Ident i) = return $ A.prim i
normalizeExpr (B.List l) = normalizeList l

normalizeList :: (Functor m, Monad m) => List B.Expr -> SchemeT m A.Expr
normalizeList (ProperList ((B.Ident "define"):(B.Ident var):[expr])) =
    A.Define var <$> normalizeExpr expr
normalizeList (ProperList ((B.Ident "define"):(B.List vars):body)) = do
    (var, args) <- splitArgs vars
    A.Define var <$> lambdaBody args body
normalizeList (ProperList ((B.Ident "define"):_)) =
    throwError $ SyntaxError "define"
normalizeList (ProperList ((B.Ident "define-macro"):(B.Ident var):[expr])) =
    A.DefineMacro var <$> normalizeExpr expr
normalizeList (ProperList ((B.Ident "define-macro"):(B.List vars):body)) = do
    (var, args) <- splitArgs vars
    A.DefineMacro var <$> lambdaBody args body
normalizeList (ProperList ((B.Ident "lambda"):(B.Ident args):body)) = do
    let args' = Args $ DottedList [] args
    lambdaBody args' body
normalizeList (ProperList ((B.Ident "lambda"):(B.List args):body)) = do
    args' <- Args <$> extractIdents args
    lambdaBody args' body
normalizeList (ProperList ((B.Ident "lambda"):_)) =
    throwError $ SyntaxError "lambda"
normalizeList (ProperList ((B.Ident "quote"):[e])) =
    A.Quote <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "quote"):_)) =
    throwError $ SyntaxError "quote"
normalizeList (ProperList ((B.Ident "quasiquote"):[e])) =
    A.QuasiQuote <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "quasiquote"):_)) =
    throwError $ SyntaxError "quasiquote"
normalizeList (ProperList ((B.Ident "unquote"):[e])) =
    A.Unquote <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "unquote"):_)) =
    throwError $ SyntaxError "unquote"
normalizeList (ProperList ((B.Ident "unquote-splicing"):[e])) =
    A.UnquoteSplicing <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "unquote-splicing"):_)) =
    throwError $ SyntaxError "unquote-splicing"
normalizeList (ProperList ((B.Ident "begin"):[])) = return A.Undefined
normalizeList (ProperList ((B.Ident "begin"):[e])) = normalizeExpr e
normalizeList (ProperList ((B.Ident "begin"):es)) =
    A.Begin <$> mapM normalizeExpr es
normalizeList (ProperList ((B.Ident "set!"):(B.Ident var):[e])) =
    A.Set var <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "set!"):_)) =
    throwError $ SyntaxError "set!"
normalizeList (ProperList ((B.Ident "if"):b:t:[f])) =
    A.If <$> normalizeExpr b <*> normalizeExpr t <*> normalizeExpr f
normalizeList (ProperList ((B.Ident "if"):b:[t])) =
    A.If <$> normalizeExpr b <*> normalizeExpr t <*> return A.Undefined
normalizeList (ProperList ((B.Ident "if"):_)) =
    throwError $ SyntaxError "if"
normalizeList (ProperList ((B.Ident "call/cc"):[e])) = callCC e
normalizeList (ProperList ((B.Ident "call/cc"):_)) =
    throwError $ SyntaxError "call/cc"
normalizeList (ProperList ((B.Ident "call-with-current-continuation"):[e])) = callCC e
normalizeList (ProperList ((B.Ident "call-with-current-continuation"):_)) =
    throwError $ SyntaxError "call/cc"
normalizeList (ProperList ((B.Ident "load"):[e])) = A.Load <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "load"):_)) =
    throwError $ SyntaxError "load"
normalizeList (ProperList (f:params)) =
    A.Apply <$> normalizeExpr f <*> mapM normalizeExpr params
normalizeList (ProperList []) = return $ A.Const Nil
normalizeList (DottedList es e) =
    A.Dot <$> mapM normalizeExpr es <*> normalizeExpr e

splitArgs :: (Functor m, Monad m) => List B.Expr -> SchemeT m (Ident, Args)
splitArgs exprs = extractIdents exprs >>= split
  where
    split (ProperList []) = throwError $ SyntaxError "define must have variable name"
    split (ProperList (name:params)) = return (name, Args $ ProperList params)
    split (DottedList [] _) = throwError $ SyntaxError "define must have variable name"
    split (DottedList (name:params) param) =
        return (name, Args $ DottedList params param)

extractIdents :: (Functor m, Monad m, Traversable t) => t B.Expr -> SchemeT m (t Ident)
extractIdents = traverse extract
  where
    extract (B.Ident i) = return i
    extract s = throwError $ SyntaxError $ show s

lambdaBody :: (Functor m, Monad m) => Args -> [B.Expr] -> SchemeT m A.Expr
lambdaBody _ [] = throwError $ SyntaxError "lambda body must be one or more expressions"
lambdaBody args [e] = A.Lambda args <$> normalizeExpr e
lambdaBody args es = A.Lambda args . A.Begin <$> mapM normalizeExpr es

callCC :: (Functor m, Monad m) => B.Expr -> SchemeT m A.Expr
callCC e = do
    e' <- normalizeExpr e
    case e' of
        A.Lambda args body -> return $ A.CallCC A.Undefined args body
        _ -> throwError $ SyntaxError "call/cc"
