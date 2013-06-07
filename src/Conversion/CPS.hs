module Conversion.CPS where

import Control.Monad
import Control.Monad.State

import Types.Syntax.After
import Types.Util
import Util

cps :: Expr -> Expr
cps e = evalState (cpsExpr e End) 1

cpsExpr :: Expr -> CC -> State Int Expr
cpsExpr c@(Const _) cc = return $ Apply cc [c]
cpsExpr v@(Var _) cc = return $ Apply cc [v]
cpsExpr (Define v e) cc = do
    var <- getVar
    e' <- cpsExpr e $ Lambda (params [var]) $ Var var
    return $ Apply cc [Define v e']
cpsExpr (DefineMacro v e) cc = do
    var <- getVar
    e' <- cpsExpr e $ Lambda (params [var]) $ Var var
    return $ Apply cc [DefineMacro v e']
cpsExpr (Lambda args e) cc = do
    var <- getVar
    e' <- cpsExpr e $ Var var
    return $ Apply cc [Lambda (consArgs var args) e']
cpsExpr f@(Func _ _ _) _ = return f
cpsExpr (Apply p@(Prim _) args) cc = do
    vars <- getVars $ length args
    go cc vars $ zip vars args
  where
    go c vars' [] = return $ Apply c [Apply p $ map Var vars']
    go c vars' ((var, arg):args') = do
        e <- go c vars' args'
        cpsExpr arg $ Lambda (params [var]) e
cpsExpr (Apply f args) cc = do
    (fvar:vars) <- getVars $ length (f:args)
    go cc fvar vars $ zip (fvar:vars) (f:args)
  where
    go c fvar vars [] =
        return $ Apply (Var fvar) $ c : map Var vars
    go c fvar vars ((var, arg):args') = do
        e <- go c fvar vars args'
        cpsExpr arg $ Lambda (params [var]) e
cpsExpr (Dot es e) cc = return $ Apply cc [Dot es e]
cpsExpr (CallCC _ args body) cc = do
    var <- getVar
    n <- get
    let (cc', n') = runState (cpsExpr cc $ Lambda (params [var]) (Var var)) $ n + 1
    put n'
    body' <- cpsExpr body cc
    return $ CallCC cc' args body'
cpsExpr p@(Prim _) _ = return p
cpsExpr (Quote e) cc = return $ Apply cc [Quote e]
cpsExpr (QuasiQuote e) cc = return $ Apply cc [QuasiQuote e]
cpsExpr (Unquote e) cc = return $ Apply cc [Unquote e]
cpsExpr (UnquoteSplicing e) cc = return $ Apply cc [UnquoteSplicing e]
cpsExpr (Begin []) _ = return Undefined
cpsExpr (Begin [e]) cc = cpsExpr e cc
cpsExpr (Begin (e:es)) cc = do
    var <- getVar
    e' <- cpsExpr (Begin es) cc
    cpsExpr e $ Lambda (params [var]) e'
cpsExpr (Set v e) cc = do
    var <- getVar
    cpsExpr e $ Lambda (params [var]) $ Apply cc [Set v $ Var var]
cpsExpr (If b t f) cc = do
    var <- getVar
    e1 <- cpsExpr t cc
    e2 <- cpsExpr f cc
    cpsExpr b $ Lambda (params [var]) $ If (Var var) e1 e2
cpsExpr (Load e) cc = do
    var <- getVar
    e' <- cpsExpr e $ Lambda (params [var]) $ Var var
    return $ Apply cc [Load e']
cpsExpr Undefined cc = return $ Apply cc [Undefined]
cpsExpr End cc = return $ Apply cc [End]

getVar :: State Int Ident
getVar = do
    n <- get
    modify succ
    return $ "##" ++ show n

getVars :: Int -> State Int [Ident]
getVars n = replicateM n getVar

params :: [Ident] -> Args
params = Args . ProperList
