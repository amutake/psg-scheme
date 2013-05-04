{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Monad.Trans.Resource (MonadThrow (..))

import Types

eval :: (MonadThrow m) => Value -> SchemeT m Value
eval b@(Bool _) = return b
eval n@(Number _) = return n
eval s@(String _) = return s
eval Nil = return Nil
eval (Pair (Ident "quote") (Pair cdr Nil)) = return cdr
eval (Pair (Ident "exit") _) = monadThrow Exit
eval p@(Pair _ _) = monadThrow $ Undefined $ show p
eval (Ident i) = monadThrow $ Undefined i
