{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Applicative ((<$>))
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (readIORef, modifyIORef)
import qualified Data.Map as Map

import Types

lookupEnv :: MonadBase IO m => EnvRef -> Ident -> SchemeT m Value
lookupEnv ref ident = do
    env <- readIORef ref
    case env of
        Global m -> case Map.lookup ident m of
            Nothing -> throwIO $ Undefined ident
            Just v -> return v
        Extended m ref' -> case Map.lookup ident m of
            Nothing -> lookupEnv ref' ident
            Just v -> return v

define :: MonadBase IO m => EnvRef -> Ident -> Value -> SchemeT m Value
define ref ident val = do
    modifyIORef ref (insert ident val)
    return $ Ident ident

insert :: Ident -> Value -> Env -> Env
insert ident val (Global m) = Global $ Map.insert ident val m
insert ident val (Extended m r) = Extended (Map.insert ident val m) r

splitIdents :: MonadBase IO m => List Value -> SchemeT m (Ident, List Ident)
splitIdents (ProperList []) = throwIO $ Undefined "syntax error"
splitIdents (ProperList idents) = do
    name:params <- extractIdents idents
    return (name, ProperList params)
splitIdents (DottedList [] _) = throwIO $ Undefined "syntax error"
splitIdents (DottedList idents param) = do
    param':[] <- extractIdents [param]
    name:params <- extractIdents idents
    return (name, DottedList params param')

extractIdents :: MonadBase IO m => [Value] -> SchemeT m [Ident]
extractIdents [] = return []
extractIdents ((Ident s):vals) = (s:) <$> extractIdents vals
extractIdents (s:_) = throwIO $ Undefined $ show s
