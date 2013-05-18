{-# LANGUAGE FlexibleContexts #-}

module Conversion.Normalize where

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.Traversable (Traversable, traverse)

import Types.Exception
import qualified Types.Syntax.After as A
import qualified Types.Syntax.Before as B
import Types.Util

normalize :: MonadBase IO m => B.Expr -> m A.Expr
normalize expr = normalizeExpr $ flattenList expr

flattenList :: B.Expr -> B.Expr
flattenList (B.List (DottedList xs x)) = case x of
    B.List (ProperList ys) -> B.List $ ProperList $ map flattenList $ xs ++ ys
    B.List (DottedList ys y) -> B.List $ DottedList (map flattenList $ xs ++ ys) y
    y -> B.List $ DottedList xs y
flattenList v = v

normalizeExpr :: MonadBase IO m => B.Expr -> m A.Expr
normalizeExpr (B.Const c) = return $ A.Const c
normalizeExpr (B.Ident i) = return $ prim i
normalizeExpr (B.List l) = normalizeList l

normalizeList :: MonadBase IO m => List B.Expr -> m A.Expr
normalizeList (ProperList ((B.Ident "define"):(B.Ident var):[expr])) =
    A.Define var <$> normalizeExpr expr
normalizeList (ProperList ((B.Ident "define"):(B.List vars):body)) = do
    (var, args) <- splitArgs vars
    A.Define var <$> lambdaBody args body
normalizeList (ProperList ((B.Ident "define"):_)) =
    throwIO $ SyntaxError "define"
normalizeList (ProperList ((B.Ident "lambda"):(B.Ident args):body)) = do
    let args' = A.Args $ DottedList [] args
    lambdaBody args' body
normalizeList (ProperList ((B.Ident "lambda"):(B.List args):body)) = do
    args' <- A.Args <$> extractIdents args
    lambdaBody args' body
normalizeList (ProperList ((B.Ident "lambda"):_)) =
    throwIO $ SyntaxError "lambda"
normalizeList (ProperList ((B.Ident "quote"):[e])) =
    A.Quote <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "quote"):_)) =
    throwIO $ SyntaxError "quote"
normalizeList (ProperList ((B.Ident "begin"):[])) = return A.Undefined
normalizeList (ProperList ((B.Ident "begin"):[e])) = normalizeExpr e
normalizeList (ProperList ((B.Ident "begin"):es)) =
    A.Begin <$> mapM normalizeExpr es
normalizeList (ProperList ((B.Ident "set!"):(B.Ident var):[e])) =
    A.Set var <$> normalizeExpr e
normalizeList (ProperList ((B.Ident "set!"):_)) =
    throwIO $ SyntaxError "set!"
normalizeList (ProperList ((B.Ident "if"):b:t:[f])) =
    A.If <$> normalizeExpr b <*> normalizeExpr t <*> normalizeExpr f
normalizeList (ProperList ((B.Ident "if"):b:[t])) =
    A.If <$> normalizeExpr b <*> normalizeExpr t <*> return A.Undefined
normalizeList (ProperList ((B.Ident "if"):_)) =
    throwIO $ SyntaxError "if"
normalizeList (ProperList (f:params)) =
    A.Apply <$> normalizeExpr f <*> mapM normalizeExpr params
normalizeList (ProperList []) = return $ A.Const Nil
normalizeList (DottedList _ _) = throwIO $ SyntaxError "dotted list"

splitArgs :: MonadBase IO m => List B.Expr -> m (Ident, A.Args)
splitArgs exprs = extractIdents exprs >>= split
  where
    split (ProperList []) = throwIO $ SyntaxError "define must have variable name"
    split (ProperList (name:params)) = return (name, A.Args $ ProperList params)
    split (DottedList [] _) = throwIO $ SyntaxError "define must have variable name"
    split (DottedList (name:params) param) =
        return (name, A.Args $ DottedList params param)

extractIdents :: (MonadBase IO m, Traversable t) => t B.Expr -> m (t Ident)
extractIdents = traverse extract
  where
    extract (B.Ident i) = return i
    extract s = throwIO $ SyntaxError $ show s

lambdaBody :: MonadBase IO m => A.Args -> [B.Expr] -> m A.Expr
lambdaBody _ [] = throwIO $ SyntaxError "lambda body must be one or more expressions"
lambdaBody args [e] = A.Lambda args <$> normalizeExpr e
lambdaBody args es = A.Lambda args . A.Begin <$> mapM normalizeExpr es

prim :: Ident -> A.Expr
prim "+" = A.Prim A.Add
prim "-" = A.Prim A.Sub
prim "*" = A.Prim A.Mul
prim "/" = A.Prim A.Div
prim "=" = A.Prim A.Equal
prim v = A.Var v
