{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)

import Env
import Types.Syntax.After
import Util

eval :: MonadBase IO m => EnvRef -> Expr -> m Expr
eval _ c@(Const _) = return c
eval ref (Var v) = lookupEnv ref v
eval ref (Define v e) = eval ref e >>= define ref v
eval _ l@(Lambda _ _) = return l
eval ref (Apply f es) = eval ref f >>= \f' -> apply ref f' es
eval _ p@(Prim _) = return p
eval _ (Quote e) = return e
eval ref (Begin es) = mapM (eval ref) es
eval ref (Set v e) = eval ref e >>= setVar ref v
eval ref (If b t f) = do
    b' <- eval ref b
    case b' of
        Bool True -> eval ref t
        _ -> eval ref b
eval _ Undefined = return Undefined

apply :: MonadBase IO m => EnvRef -> Expr -> [Expr] -> m Value
apply _ (Prim f) es = applyPrim f es
apply closure (Lambda args e) es = do
    ref <- newIORef $ Extended empty closure
    defines ref args es
    eval ref e
