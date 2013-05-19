module Conversion.CPS where

import Control.Monad
import Control.Monad.State

import Types.Syntax.After
import Types.Util

cps :: Expr -> Expr
cps e = evalState (cpsExpr e (Lambda (Args (ProperList ["##0"])) (Var "##0"))) 1

cpsExpr :: Expr -> CC -> State Int Expr
cpsExpr c@(Const _) cc = return $ Apply cc [c]
cpsExpr v@(Var _) cc = return $ Apply cc [v]
cpsExpr (Define v e) cc = do
    var <- getVar
    e' <- cpsExpr e $ Lambda (params [var]) $ Var var
    return $ Apply cc [Define v e']
cpsExpr (Lambda args e) cc = do
    var <- getVar
    e' <- cpsExpr e $ Var var
    return $ Apply cc [Lambda (cons var args) e']
  where
    cons v (Args (ProperList vs)) = params $ v : vs
    cons v (Args (DottedList vs v')) = Args $ DottedList (v : vs) v'
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
cpsExpr p@(Prim _) _ = return p
cpsExpr (Quote e) cc = return $ Apply cc [e]
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
cpsExpr Undefined _ = return Undefined

getVar :: State Int Ident
getVar = do
    n <- get
    modify succ
    return $ "##" ++ show n

getVars :: Int -> State Int [Ident]
getVars n = replicateM n getVar

params :: [Ident] -> Args
params = Args . ProperList
