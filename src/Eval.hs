{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)

import Env
import Types.Env
import Types.Exception
import Types.Syntax.After
import Types.Util

eval :: MonadBase IO m => EnvRef -> Expr -> m Expr
eval _ c@(Const _) = return c
eval ref (Var v) = lookupEnv ref v
eval ref (Define v e) = eval ref e >>= define ref v
eval _ l@(Lambda _ _) = return l
eval ref (Apply f es) = eval ref f >>= \f' -> apply ref f' es
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

apply :: MonadBase IO m => EnvRef -> Expr -> [Expr] -> m Expr
apply _ (Prim f) es = applyPrim f es
apply closure (Lambda args e) es = do
    ref <- newIORef $ Extended empty closure
    defines ref args es
    eval ref e
apply _ e _ = throwIO $ NotFunction $ show e

applyPrim :: MonadBase IO m => Prim -> [Expr] -> m Expr
applyPrim = undefined
