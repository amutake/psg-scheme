{-# LANGUAGE FlexibleContexts #-}

module Primitives where

import Control.Exception.Lifted (throwIO)
import Control.Monad (foldM)
import Control.Monad.Base (MonadBase)

import Types.Exception
import Types.Syntax.After
import Types.Util


primAdd :: MonadBase IO m => [Expr] -> m Expr
primAdd = foldM add (Const $ Number 0)
  where
    add (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n + m
    add _ _ = throwIO $ TypeMismatch "Number"

primSub :: MonadBase IO m => [Expr] -> m Expr
primSub [] = throwIO $ NumArgs "at least one argument"
primSub (x:xs) = foldM sub x xs
  where
    sub (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n - m
    sub _ _ = throwIO $ TypeMismatch "Number"

primMul :: MonadBase IO m => [Expr] -> m Expr
primMul = foldM mul (Const (Number 1))
  where
    mul (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n * m
    mul _ _ = throwIO $ TypeMismatch "Number"

primDiv :: MonadBase IO m => [Expr] -> m Expr
primDiv [] = throwIO $ NumArgs "at least one argument"
primDiv ((Const (Number n)):[]) = return $ Const $ Number $ 1 `div` n
primDiv (_:[]) = throwIO $ TypeMismatch "Number"
primDiv (x:xs) = foldM div' x xs
  where
    div' (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n `div` m
    div' _ _ = throwIO $ TypeMismatch "Number"

primEqual :: MonadBase IO m => [Expr] -> m Expr
primEqual [] = throwIO $ NumArgs "at least two argument"
primEqual (_:[]) = throwIO $ NumArgs "at least two argument"
primEqual (x:xs)
    | all isNumber (x:xs) = return $ Const $ Bool $ all (== x) xs
    | otherwise = throwIO $ TypeMismatch "Number"
  where
    isNumber (Const (Number _)) = True
    isNumber _ = False
