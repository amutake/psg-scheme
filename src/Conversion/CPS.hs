module Conversion.CPS where

import Control.Monad
import Control.Monad.State

import Types.Syntax
import Util

cps :: Expr -> Expr
cps e = evalState (cpsExpr e (Evaled Return)) 1

cpsExpr :: Expr -> CC -> State Int Expr
cpsExpr c@(Const _) cc = return $ list [cc, c]
cpsExpr v@(Var _) cc = return $ list [cc, v]
cpsExpr (List (ProperList es)) cc = cpsList es cc
cpsExpr (List (DottedList es e)) cc = return $ list [cc, List $ DottedList es e]
cpsExpr e@(Evaled _) cc = return $ list [cc, e]

cpsList :: [Expr] -> CC -> State Int Expr
cpsList [Ident "define", Ident v, e] cc = do
    var <- getVar
    e' <- cpsExpr e $ listIdent "lambda" [list [var], Ident var]
    return $ list [cc, listIdent "define" [v e']]
cpsList [Ident "define-macro", Ident v, e] cc = do
    var <- getVar
    e' <- cpsExpr e $ listIdent "labmda" [list [var], Ident var]
    return $ list [cc, listIdent "define-macro" [v e']]
cpsList [Ident "lambda", args, e] cc = do
    var <- getVar
    e' <- cpsExpr e $ Ident var
    return $ list [cc, listIdent "lambda" [consArgs var args, e]]
cpsList ((Normalized (Prim _)) : args) cc = do
    vars <- getVars $ length args
    go cc vars $ zip vars args
  where
    go c vars' [] = return $ list [c, list (p : map Ident vars')]
    go c vars' ((var, arg) : args') = do
        e <- go c vars' args'
        cpsExpr arg $ listIdent "lambda" [list [var], e]
cpsList [Ident "call/cc", e] cc = do
    var <- getVar
    n <- get
    let (cc', n') = runState (cpsExpr cc $ listIdent "lambda" [list [Ident var], Ident var]) $ succ n
    put n'
    return $ [cc, list [e, cc']]
cpsList [Ident "quote", e] cc = return $ list [cc, listIdent "quote" [e]]
cpsList [Ident "quasi-quote", e] cc = return $ list [cc, listIdent "quasi-quote" [e]]
cpsList [Ident "unquote", e] cc = return $ list [cc, listIdent "unquote" [e]]
cpsList [Ident "unquote-splicing", e] cc = return $ list [cc, listIdent "unquote-splicing" [e]]
cpsList ((Ident "begin") : e : es) cc = do
    var <- getVar
    e' <- cpsExpr ((Ident "begin") : es) cc
    cpsExpr e $ listIdent "lambda" [list [Ident var], e']
cpsList [Ident "set!", Ident v, e] cc = do
    var <- getVar
    cpsExpr e $ listIdent "lambda" [list [Ident var], list [cc, listIdent "set!" [Ident var]]]
cpsList [Ident "if", b, t, f] cc = do
    var <- getVar
    t' <- cpsExpr t cc
    f' <- cpsExpr f cc
    cpsExpr b $ listIdent "lambda" [list [Ident var], listIdent "if" [Ident var, t', f']]
cpsList [] cc = return $ list [cc, list []]
cpsList (f : args) cc = do
    (fvar : vars) <- getVars $ length (f : args)
    go cc fvar vars $ zip (fvar : vars) (f : args)
  where
    go c fvar vars [] =
        return $ list fvar (c : map Ident vars)
    go c fvar vars ((var, arg) : args') = do
        e <- go c fvar vars args'
        cpsExpr arg $ listIdent "lambda" [list [var], e]

getVar :: State Int Ident
getVar = do
    n <- get
    modify succ
    return $ "##" ++ show n

getVars :: Int -> State Int [Ident]
getVars n = replicateM n getVar

list :: [Expr] -> Expr
list = List . ProperList

listIdent :: Ident -> [Expr] -> Expr
listIdent i es = list $ Ident i : es
