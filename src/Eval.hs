{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)

import Env
import Types

eval :: (MonadBase IO m) => EnvRef -> Value -> SchemeT m Value
eval _ b@(Bool _) = return b
eval _ n@(Number _) = return n
eval _ s@(String _) = return s
eval env (List l) = evalList env l
eval _ f@(Func _) = return f
eval env (Ident i) = do
    v <- lookupEnv env i
    return v

evalList :: (MonadBase IO m) => EnvRef -> List Value -> SchemeT m Value
evalList _ (ProperList ((Ident "quote"):[v])) = return v
evalList _ (ProperList ((Ident "exit"):_)) = throwIO Exit
evalList env (ProperList ((Ident "define"):(Ident id'):[v])) = do
    v' <- eval env v
    define env id' v'
evalList env (ProperList ((Ident "define"):(List ids):body)) = do
    (ident, params) <- splitIdents ids
    define env ident $ Func (Lambda params body env)
evalList _ xs@(ProperList _) = throwIO $ Undefined $ show xs
evalList _ xs@(DottedList _ _) = throwIO $ Undefined $ show xs
