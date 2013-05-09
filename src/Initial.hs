{-# LANGUAGE FlexibleContexts #-}

module Initial where

import Control.Exception.Lifted (throwIO)
import Control.Monad (foldM)
import Control.Monad.Base (MonadBase)
import Data.Map (Map, fromList)

import Types

initialEnv :: Env
initialEnv = Global primitives

primitives :: Map Ident Value
primitives = fromList
    [ ("+", Func $ Primitive primPlus)
    , ("-", Func $ Primitive primMinus)
    , ("*", Func $ Primitive primMult)
    , ("/", Func $ Primitive primDiv)
    ]

primPlus :: MonadBase IO m => [Value] -> m Value
primPlus = foldM plus (Number 0)
  where
    plus (Number n) (Number m) = return $ Number $ n + m
    plus _ _ = throwIO $ TypeMismatch "Number"

primMinus :: MonadBase IO m => [Value] -> m Value
primMinus [] = throwIO $ Undefined "at least one argument"
primMinus (x:xs) = foldM minus x xs
  where
    minus (Number n) (Number m) = return $ Number $ n - m
    minus _ _ = throwIO $ TypeMismatch "Number"

primMult :: MonadBase IO m => [Value] -> m Value
primMult = foldM mult (Number 1)
  where
    mult (Number n) (Number m) = return $ Number $ n * m
    mult _ _ = throwIO $ TypeMismatch "Number"

primDiv :: MonadBase IO m => [Value] -> m Value
primDiv [] = throwIO $ Undefined "at least one argument"
primDiv ((Number n):[]) = return $ Number $ 1 `div` n
primDiv (_:[]) = throwIO $ TypeMismatch "Number"
primDiv (x:xs) = foldM div' x xs
  where
    div' (Number n) (Number m) = return $ Number $ n `div` m
    div' _ _ = throwIO $ TypeMismatch "Number"
