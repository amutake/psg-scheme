module Conversion.CPS where

import Control.Monad
import Control.Monad.State

import Syntax.After
import Types (List (..), Ident)

cps :: Expr -> CC -> State Int Expr
cps c@(Const _) cc = return $ Apply cc [c]
cps v@(Var _) cc = return $ Apply cc [v]
cps (Define v e) cc = do
    var <- getVar
    cps e $ Lambda (params [var]) $ Apply cc [Define v $ Var var]
cps (Lambda args e) cc = do
    var <- getVar
    e' <- cps e $ Var var
    return $ Apply cc [Lambda (cons var args) e']
  where
    cons v (Args (ProperList vs)) = params $ v : vs
    cons v (Args (DottedList vs v')) = Args $ DottedList (v : vs) v'
cps (Apply p@(Prim _) args) cc = do
    vars <- getVars $ length args
    go cc vars $ zip vars args
  where
    go c vars' [] = return $ Apply c [Apply p $ map Var vars']
    go c vars' ((var, arg):args') = do
        e <- go c vars' args'
        cps arg $ Lambda (params [var]) e
cps (Apply f args) cc = do
    (fvar:vars) <- getVars $ length (f:args)
    go cc fvar vars $ zip (fvar:vars) (f:args)
  where
    go c fvar vars [] =
        return $ Apply (Var fvar) $ c : map Var vars
    go c fvar vars ((var, arg):args') = do
        e <- go c fvar vars args'
        cps arg $ Lambda (params [var]) e
cps p@(Prim _) _ = return p
cps (Quote e) cc = return $ Apply cc [e]
cps (Begin []) _ = return Undefined
cps (Begin [e]) cc = cps e cc
cps (Begin (e:es)) cc = do
    var <- getVar
    e' <- cps (Begin es) cc
    cps e $ Lambda (params [var]) e'
cps (Set v e) cc = do
    var <- getVar
    cps e $ Lambda (params [var]) $ Apply cc [Set v $ Var var]
cps (If b t f) cc = do
    var <- getVar
    e1 <- cps t cc
    e2 <- cps f cc
    cps b $ Lambda (params [var]) $ If (Var var) e1 e2
cps Undefined _ = return Undefined

getVar :: State Int Ident
getVar = do
    n <- get
    modify succ
    return $ "##" ++ show n

getVars :: Int -> State Int [Ident]
getVars n = replicateM n getVar

params :: [Ident] -> Args
params = Args . ProperList
