{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Monad.Trans.Resource (MonadThrow (..))

import Types

eval :: (MonadThrow m) => Value -> SchemeT m Value
eval b@(Bool _) = return b
eval n@(Number _) = return n
eval s@(String _) = return s
eval (ProperList ((Ident "quote"):[v])) = return v
eval (ProperList ((Ident "exit"):_)) = monadThrow Exit
eval xs@(ProperList _) = monadThrow $ Undefined $ show xs
eval xs@(DottedList _ _) = monadThrow $ Undefined $ show xs
eval (Ident i) = monadThrow $ Undefined i
