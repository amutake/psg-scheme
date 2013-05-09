{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)

import Types

eval :: (MonadBase IO m) => Value -> SchemeT m Value
eval b@(Bool _) = return b
eval n@(Number _) = return n
eval s@(String _) = return s
eval (ProperList ((Ident "quote"):[v])) = return v
eval (ProperList ((Ident "exit"):_)) = throwIO Exit
eval xs@(ProperList _) = throwIO $ Undefined $ show xs
eval xs@(DottedList _ _) = throwIO $ Undefined $ show xs
eval (Ident i) = throwIO $ Undefined i
