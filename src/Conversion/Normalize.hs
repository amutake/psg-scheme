{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Conversion.Normalize where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (throwError)

import Types.Core
import Types.Exception
import Types.Syntax
import Util

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
normalizeExpr (Ident i) = return $ prim i
normalizeExpr (List l) = normalizeList l
normalizeExpr n@(Normalized _) = return n
normalizeExpr e@(Evaled _) = return e

normalizeList :: MonadScheme m => List Expr -> SchemeT m Expr
normalizeList (ProperList [Ident "define", Ident var, expr]) = do
    e <- normalizeExpr expr
    construct "define" [Ident var, e]
normalizeList (ProperList ((Ident "define"):(List vars):body)) = do
    (var, args) <- splitArgs vars
    l <- lambdaBody (List $ fmap Ident args) body
    construct "define" [Ident var, l]
normalizeList (ProperList ((Ident "define"):_)) =
    throwError $ SyntaxError "define"
normalizeList (ProperList ((Ident "define-macro"):(Ident var):[expr])) = do
    e <- normalizeExpr expr
    construct "define-macro" [Ident var, e]
normalizeList (ProperList ((Ident "define-macro"):(List vars):body)) = do
    (var, args) <- splitArgs vars
    l <- lambdaBody (List $ fmap Ident args) body
    construct "define-macro" [Ident var, l]
normalizeList (ProperList ((Ident "lambda"):(Ident args):body)) = do
    let args' = List $ DottedList [] $ Ident args
    lambdaBody args' body
normalizeList (ProperList ((Ident "lambda"):(List args):body)) = do
    args' <- normalizeExpr $ List args
    lambdaBody args' body
normalizeList (ProperList ((Ident "lambda"):_)) =
    throwError $ SyntaxError "lambda"
normalizeList (ProperList [Ident "begin"]) = return $ Const Undefined
normalizeList (ProperList [Ident "begin", List (ProperList [Ident "unquote-splicing", e])]) = do
    e' <- normalizeExpr e
    construct "begin" [List $ ProperList [Ident "unquote-splicing", e']]
normalizeList (ProperList [Ident "begin", e]) = normalizeExpr e
normalizeList (ProperList ((Ident "begin"):es)) = do
    es' <- mapM normalizeExpr es
    construct "begin" es'
normalizeList (ProperList [Ident "set!", Ident var, e]) = do
    e' <- normalizeExpr e
    construct "set!" [Ident var, e']
-- normalizeList (ProperList ((Ident "set!"):_)) =
--     throwError $ SyntaxError "set!"
normalizeList (ProperList [Ident "if", b, t, f]) = do
    b' <- normalizeExpr b
    t' <- normalizeExpr t
    f' <- normalizeExpr f
    construct "if" [b', t', f']
normalizeList (ProperList [Ident "if", b , t]) = do
    b' <- normalizeExpr b
    t' <- normalizeExpr t
    construct "if" [b', t', Const Undefined]
normalizeList (ProperList (Ident "if" : _)) =
    throwError $ SyntaxError "if"
normalizeList (ProperList [Ident "load", e]) = do
    e' <- normalizeExpr e
    construct "load" [e']
normalizeList (ProperList ((Ident "load") : _)) =
    throwError $ SyntaxError "load"
normalizeList (ProperList ((Ident "call-with-current-continuation"):es)) =
    construct "call/cc" es
normalizeList (ProperList es) =
    List <$> ProperList <$> mapM normalizeExpr es
normalizeList (DottedList es e) =
    List <$> (DottedList <$> mapM normalizeExpr es <*> normalizeExpr e)

splitArgs :: MonadScheme m => List Expr -> SchemeT m (Ident, List Ident)
splitArgs exprs = extractIdents exprs >>= split
  where
    split (ProperList []) = throwError $ SyntaxError "define must have variable name"
    split (ProperList (name:params)) = return (name, ProperList params)
    split (DottedList [] _) = throwError $ SyntaxError "define must have variable name"
    split (DottedList (name:params) param) =
        return (name, DottedList params param)

lambdaBody :: MonadScheme m => Expr -> [Expr] -> SchemeT m Expr
lambdaBody _ [] = throwError $ SyntaxError "lambda body must be one or more expressions"
lambdaBody args es = do
    e <- normalizeList $ ProperList $ Ident "begin" : es
    construct "lambda" $ args : [e]

construct :: MonadScheme m => Ident -> [Expr] -> SchemeT m Expr
construct i es = return $ List $ ProperList $ Ident i : es

prim :: Ident -> Expr
prim "+" = Normalized $ Prim Add
prim "-" = Normalized $ Prim Sub
prim "*" = Normalized $ Prim Mul
prim "/" = Normalized $ Prim Div
prim "=" = Normalized $ Prim Equal
prim "<" = Normalized $ Prim NLT
prim ">" = Normalized $ Prim NGT
prim "eqv?" = Normalized $ Prim Eqv
prim "car" = Normalized $ Prim Car
prim "cdr" = Normalized $ Prim Cdr
prim "cons" = Normalized $ Prim Cons
prim "pair?" = Normalized $ Prim Pair
prim "number?" = Normalized $ Prim NumberP
prim "symbol?" = Normalized $ Prim SymbolP
prim "boolean?" = Normalized $ Prim BooleanP
prim "string?" = Normalized $ Prim StringP
prim v = Ident v
