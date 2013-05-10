{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Applicative ((<$>))
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (readIORef, modifyIORef)
import qualified Data.Map as Map

import Types

lookupEnv :: MonadBase IO m => EnvRef -> Ident -> m Value
lookupEnv ref ident = do
    env <- readIORef ref
    case env of
        Global m -> case Map.lookup ident m of
            Nothing -> throwIO $ Undefined ident
            Just v -> return v
        Extended m ref' -> case Map.lookup ident m of
            Nothing -> lookupEnv ref' ident
            Just v -> return v

define :: MonadBase IO m => EnvRef -> Ident -> Value -> m Value
define ref ident val = do
    modifyIORef ref (insert ident val)
    return $ Ident ident

defines :: MonadBase IO m => EnvRef -> List Ident -> [Value] -> m ()
defines ref (ProperList idents) vals
    | length idents == length vals = do
        modifyIORef ref $ union $ Map.fromList $ zip idents vals
    | otherwise = throwIO $ Undefined "num args"
defines ref (DottedList idents ident) vals
    | length idents > length vals = throwIO $ Undefined "num args"
    | otherwise = do
        let (init', last') = splitAt (length idents) vals
        modifyIORef ref $ union $ Map.fromList $ zip idents init'
        modifyIORef ref $ insert ident $ List $ ProperList last'

insert :: Ident -> Value -> Env -> Env
insert ident val (Global m) = Global $ Map.insert ident val m
insert ident val (Extended m r) = Extended (Map.insert ident val m) r

union :: Map.Map Ident Value -> Env -> Env
union m (Global m') = Global $ Map.union m m'
union m (Extended m' r) = Extended (Map.union m m') r

splitIdents :: MonadBase IO m => List Value -> m (Ident, List Ident)
splitIdents (ProperList []) = throwIO $ Undefined "syntax error"
splitIdents (ProperList idents) = do
    name:params <- extractIdents idents
    return (name, ProperList params)
splitIdents (DottedList [] _) = throwIO $ Undefined "syntax error"
splitIdents (DottedList idents param) = do
    param':[] <- extractIdents [param]
    name:params <- extractIdents idents
    return (name, DottedList params param')

extractIdents :: MonadBase IO m => [Value] -> m [Ident]
extractIdents [] = return []
extractIdents ((Ident s):vals) = (s:) <$> extractIdents vals
extractIdents (s:_) = throwIO $ Undefined $ show s

setVar :: MonadBase IO m => EnvRef -> Ident -> Value -> m Value
setVar ref ident val = do
    env <- readIORef ref
    case env of
        Global map' -> if Map.member ident map'
            then do
                modifyIORef ref $ insert ident val
                return val
            else throwIO $ Undefined ident
        Extended map' ref' -> if Map.member ident map'
            then do
                modifyIORef ref $ insert ident val
                return val
            else setVar ref' ident val