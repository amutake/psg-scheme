{-# LANGUAGE FlexibleContexts #-}

module Primitives where

import Control.Monad (foldM)
import Control.Monad.Error (MonadError (..))

import Types.Core
import Types.Exception
import Types.Syntax.After
import Types.Util

primAdd :: Monad m => [Expr] -> SchemeT m Expr
primAdd = foldM add (Const $ Number 0)
  where
    add (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n + m
    add _ _ = throwError $ TypeMismatch "Number"

primSub :: Monad m => [Expr] -> SchemeT m Expr
primSub [] = throwError $ NumArgs "(-): 1 <= args"
primSub (x:xs) = foldM sub x xs
  where
    sub (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n - m
    sub _ _ = throwError $ TypeMismatch "Number"

primMul :: Monad m => [Expr] -> SchemeT m Expr
primMul = foldM mul (Const (Number 1))
  where
    mul (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n * m
    mul _ _ = throwError $ TypeMismatch "Number"

primDiv :: Monad m => [Expr] -> SchemeT m Expr
primDiv [] = throwError $ NumArgs "(/): 1 <= args"
primDiv ((Const (Number n)):[]) = return $ Const $ Number $ 1 `div` n
primDiv (_:[]) = throwError $ TypeMismatch "Number"
primDiv (x:xs) = foldM div' x xs
  where
    div' (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n `div` m
    div' _ _ = throwError $ TypeMismatch "Number"

primEqual :: Monad m => [Expr] -> SchemeT m Expr
primEqual [] = throwError $ NumArgs "(=): 2 <= args"
primEqual (_:[]) = throwError $ NumArgs "(=): 2 <= args"
primEqual (x:xs)
    | all isNumber (x:xs) = return $ Const $ Bool $ all (== x) xs
    | otherwise = throwError $ TypeMismatch "Number"
  where
    isNumber (Const (Number _)) = True
    isNumber _ = False

primEqv :: Monad m => [Expr] -> SchemeT m Expr
primEqv xs
    | length xs == 2 = return $ Const $ Bool $ xs !! 0 == xs !! 1
    | otherwise = throwError $ NumArgs "eqv?: args == 2"

primCar :: Monad m => [Expr] -> SchemeT m Expr
primCar ((Apply x _):[]) = return x
primCar ((Dot (x:_) _):[]) = return $ x
primCar (_:[]) = throwError $ TypeMismatch "pair"
primCar _ = throwError $ NumArgs "car: args == 1"
