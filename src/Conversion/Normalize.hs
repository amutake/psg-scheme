{-# LANGUAGE FlexibleContexts #-}

module Conversion.Normalize where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (throwError)
import Data.Traversable (Traversable, traverse)

import Types.Core
import Types.Exception
import Types.Syntax

normalize :: MonadScheme m => Expr -> SchemeT m Expr
normalize expr = normalizeExpr $ flattenList expr

flattenList :: Expr -> Expr
flattenList (List (DottedList xs x)) = case x of
    List (ProperList ys) -> List $ ProperList $ map flattenList $ xs ++ ys
    List (DottedList ys y) -> List $ DottedList (map flattenList $ xs ++ ys) y
    _ -> List $ DottedList xs x
flattenList v = v

normalizeExpr :: MonadScheme m => Expr -> SchemeT m Expr
normalizeExpr c@(Const _) = return c
normalizeExpr i@(Ident _) = return i
normalizeExpr (List l) = normalizeList l
normalizeExpr e@(Evaled _) = return e

normalizeList :: MonadScheme m => List Expr -> SchemeT m Expr
normalizeList (ProperList ((Ident "define"):(Ident var):[expr])) =
    e <- normalizeExpr expr
    construct "define" [Ident var, e]
normalizeList (ProperList ((Ident "define"):(List vars):body)) = do
    (var, args) <- splitArgs vars
    l <- lambdaBody args body
    construct "define" [Ident var, l]
normalizeList (ProperList ((Ident "define"):_)) =
    throwError $ SyntaxError "define"
normalizeList (ProperList ((Ident "define-macro"):(Ident var):[expr])) =
    e <- normalizeExpr expr
    construct "define-macro" [Ident var, e]
normalizeList (ProperList ((Ident "define-macro"):(List vars):body)) = do
    (var, args) <- splitArgs vars
    l <- lambdaBody args body
    construct "define-macro" [Ident var, l]
normalizeList (ProperList ((Ident "lambda"):(Ident args):body)) = do
    let args' = DottedList [] args
    lambdaBody args' body
normalizeList (ProperList ((Ident "lambda"):(List args):body)) = do
    args' <- extractIdents args
    lambdaBody args' body
normalizeList (ProperList ((Ident "lambda"):_)) =
    throwError $ SyntaxError "lambda"
normalizeList (ProperList [Ident "begin"]) = return Undefined
normalizeList (ProperList [Ident "begin", e]) = normalizeExpr e
normalizeList (ProperList ((Ident "begin"):es)) =
    es' <- mapM normalizeExpr es
    construct "begin" es'
normalizeList (ProperList [Ident "set!", Ident var, e])) =
    e' <- normalizeExpr e
    construct "set!" [Ident var, e']
normalizeList (ProperList ((Ident "set!"):_)) =
    throwError $ SyntaxError "set!"
normalizeList (ProperList [Ident "if", b, t, f]) =
    b' <- normalizeExpr b
    t' <- normalizeExpr t
    f' <- normalizeExpr f
    construct "if" [b', t', f']
normalizeList (ProperList [Ident "if", b , t]) =
    If <$> normalizeExpr b <*> normalizeExpr t <*> return Undefined
normalizeList (ProperList (Ident "if" : _)) =
    throwError $ SyntaxError "if"
normalizeList (ProperList ((Ident "call-with-current-continuation"):es)) =
    construct "call/cc" es
normalizeList (ProperList es) =
    List <$> ProperList <*> mapM normalizeExpr es
normalizeList (DottedList es e) =
    List <$> ProperList <*> mapM normalizeExpr es <*> normalizeExpr e

splitArgs :: MonadScheme => List Expr -> SchemeT m (Ident, List Ident)
splitArgs exprs = extractIdents exprs >>= split
  where
    split (ProperList []) = throwError $ SyntaxError "define must have variable name"
    split (ProperList (name:params)) = return (name, List $ ProperList params)
    split (DottedList [] _) = throwError $ SyntaxError "define must have variable name"
    split (DottedList (name:params) param) =
        return (name, List $ DottedList params param)

extractIdents :: (MonadScheme m, Traversable t) => t Expr -> SchemeT m (t Ident)
extractIdents = traverse extract
  where
    extract (Ident i) = return i
    extract s = throwError $ SyntaxError $ show s

lambdaBody :: MonadScheme m => List Expr -> [Expr] -> SchemeT m Expr
lambdaBody _ [] = throwError $ SyntaxError "lambda body must be one or more expressions"
lambdaBody args es = do
    e <- normalizeList $ List $ ProperList $ Ident "begin" : es
    construct "lambda" $ List args : [e]

construct :: MonadScheme m => Ident -> [Expr] -> SchemeT m Expr
construct i es = return $ List $ ProperList $ Ident i : es
