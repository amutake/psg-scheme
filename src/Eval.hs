{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)

import Types

eval :: (MonadBase IO m) => Value -> SchemeT m Value
eval b@(Bool _) = return b
eval n@(Number _) = return n
eval s@(String _) = return s
eval (List l) = evalList l
eval f@(Func _) = return f
eval (Ident i) = throwIO $ Undefined i

evalList :: (MonadBase IO m) => List Value -> SchemeT m Value
evalList (ProperList ((Ident "quote"):[v])) = return v
evalList (ProperList ((Ident "exit"):_)) = throwIO Exit
evalList xs@(ProperList _) = throwIO $ Undefined $ show xs
evalList xs@(DottedList _ _) = throwIO $ Undefined $ show xs
