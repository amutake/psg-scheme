{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (readIORef, modifyIORef)
import qualified Data.Map as Map

import Types

lookupEnv :: MonadBase IO m => EnvRef -> Ident -> m Value
lookupEnv ref ident = readIORef ref >>= lookupEnv'
  where
    lookupEnv' (Global map') =
        maybe (throwIO $ Undefined ident) return $ Map.lookup ident map'
    lookupEnv' (Extended map' ref') =
        maybe (lookupEnv ref' ident) return $ Map.lookup ident map'

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

setVar :: MonadBase IO m => EnvRef -> Ident -> Value -> m Value
setVar ref ident val = readIORef ref >>= set
  where
    set (Global map') = set' map' $ throwIO $ Undefined ident
    set (Extended map' ref') = set' map' $ setVar ref' ident val
    set' map' els
        | Map.member ident map' = do
              modifyIORef ref $ insert ident val
              return val
        | otherwise = els
