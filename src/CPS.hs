module CPS where

import Control.Monad
import Control.Monad.State

import Types.Syntax
import Util

cps :: Expr -> Expr
cps e = evalState (cpsExpr e [Evaled Return]) 1

cpsExpr :: Expr -> [CC] -> State Int Expr
cpsExpr c@(Const _) cc = return $ list [head cc, c]
cpsExpr i@(Ident _) cc = return $ list [head cc, i]
cpsExpr (List (ProperList es)) cc = cpsList es cc
cpsExpr (List (DottedList es e)) cc = return $ list [head cc, List $ DottedList es e]
cpsExpr n@(Normalized _) cc = return $ list [head cc, n]
cpsExpr e@(Evaled _) cc = return $ list [head cc, e]

cpsList :: [Expr] -> [CC] -> State Int Expr
cpsList [Ident "define", v, e] cc = do
    var <- getVar
    e' <- cpsExpr e $ listIdent "lambda" [list [Ident var], Ident var] : cc
    return $ list [head cc, listIdent "define" [v, e']]
cpsList [Ident "define-macro", v, e] cc = do
    var <- getVar
    e' <- cpsExpr e $ listIdent "lambda" [list [Ident var], Ident var] : cc
    return $ list [head cc, listIdent "define-macro" [v, e']]
cpsList [Ident "lambda", List args, e] cc = do
    var <- getVar
    e' <- cpsExpr e $ Ident var : cc
    return $ list [head cc, listIdent "lambda" [List $ cons (Ident var) args, e']]
cpsList [Ident "call/cc", f] cc = do
    var <- getVar
    dummy <- getVar
    result <- getVar
    cpsExpr f $ listIdent "lambda" [list [Ident var], list [Ident var, head cc, listIdent "lambda" [list [Ident dummy, Ident result], list [head cc, Ident result]]]] : tail cc
cpsList [Ident "reset", e] cc = do
    var <- getVar
    cpsExpr e $ listIdent "lambda" [list [Ident var], Ident var] : cc
cpsList [Ident "shift", f] cc = do
    var <- getVar
    gc <- getVar
    result <- getVar
    cpsExpr f $ listIdent "lambda" [list [Ident var], list [Ident var, cc !! 1, listIdent "lambda" [list [Ident gc, Ident result], list [Ident gc, list [head cc, Ident result]]]]] : tail (tail cc)
cpsList [Ident "quote", e] cc = return $ list [head cc, listIdent "quote" [e]]
cpsList ((Ident "begin") : e : es) cc = do
    var <- getVar
    let es' | es == [] = List $ ProperList []
            | length es == 1 = head es
            | otherwise = List $ ProperList $ (Ident "begin") : es
    e' <- cpsExpr es' cc
    cpsExpr e $ listIdent "lambda" [list [Ident var], e'] : cc
cpsList [Ident "set!", v, e] cc = do
    var <- getVar
    cpsExpr e $ listIdent "lambda" [list [Ident var], list [head cc, listIdent "set!" [v, Ident var]]] : tail cc
cpsList [Ident "if", b, t, f] cc = do
    var <- getVar
    t' <- cpsExpr t cc
    f' <- cpsExpr f cc
    cpsExpr b $ listIdent "lambda" [list [Ident var], listIdent "if" [Ident var, t', f']] : tail cc
cpsList [Ident "load", path] cc = return $ list [head cc, listIdent "load" [path]]
cpsList [] cc = return $ list [head cc, list []]
cpsList (f : args) cc = do
    (fvar : vars) <- getVars $ length (f : args)
    go cc fvar vars $ zip (fvar : vars) (f : args)
  where
    go c fvar vars [] =
        return $ list (Ident fvar : head c : map Ident vars)
    go c fvar vars ((var, arg) : args') = do
        e <- go c fvar vars args'
        cpsExpr arg $ listIdent "lambda" [list [Ident var], e] : tail c

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
