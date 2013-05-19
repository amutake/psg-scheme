{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)

import Env
import Primitives
import Types.Exception
import Types.Syntax.After
import Types.Util

eval :: MonadBase IO m => EnvRef -> Expr -> m Expr
eval _ c@(Const _) = return c
eval ref (Var v) = lookupEnv ref v
eval ref (Define v e) = eval ref e >>= define ref v
eval ref (Lambda args body) = return $ Func args body ref
eval _ f@(Func _ _ _) = return f
eval ref (Apply f es) = do
    f' <- eval ref f
    es' <- mapM (eval ref) es
    apply f' es'
eval _ p@(Prim _) = return p
eval _ (Quote e) = return e
eval ref (Begin es) = mapM (eval ref) (init es) >> eval ref (last es)
eval ref (Set v e) = eval ref e >>= setVar ref v
eval ref (If b t f) = do
    b' <- eval ref b
    case b' of
        Const (Bool True) -> eval ref t
        _ -> eval ref f
eval _ Undefined = return Undefined

apply :: MonadBase IO m => Expr -> [Expr] -> m Expr
apply (Prim f) es = applyPrim f es
apply (Func args body closure) es = do
    ref <- newIORef $ Extended empty closure
    defines ref args es
    eval ref body
apply e _ = throwIO $ NotFunction $ show e

applyPrim :: MonadBase IO m => Prim -> [Expr] -> m Expr
applyPrim Add = primAdd
applyPrim Sub = primSub
applyPrim Mul = primMul
applyPrim Div = primDiv
applyPrim Equal = primEqual
