{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (readIORef, modifyIORef)
import qualified Data.Map as Map

import Types.Exception
import Types.Syntax.After
import Types.Util

lookupEnv :: MonadBase IO m => EnvRef -> Ident -> m Expr
lookupEnv ref var = readIORef ref >>= lookupEnv'
  where
    lookupEnv' (Global map') =
        maybe (throwIO $ Unbind var) return $ Map.lookup var map'
    lookupEnv' (Extended map' ref') =
        maybe (lookupEnv ref' var) return $ Map.lookup var map'

define :: MonadBase IO m => EnvRef -> Ident -> Expr -> m Expr
define ref var expr = do
    modifyIORef ref (insert var expr)
    return $ Var var

defines :: MonadBase IO m => EnvRef -> Args -> [Expr] -> m ()
defines ref (Args (ProperList args)) exprs
    | length args == length exprs = do
        modifyIORef ref $ union $ Map.fromList $ zip args exprs
    | otherwise = throwIO $ NumArgs $ "not match: args: " ++ show args ++ ", exprs: " ++ show exprs
defines ref (Args (DottedList args arg)) exprs
    | length args > length exprs = throwIO $ NumArgs $ "not match: args: " ++ show args ++ ", exprs: " ++ show exprs
    | otherwise = do
        let (init', last') = splitAt (length args) exprs
        modifyIORef ref $ union $ Map.fromList $ zip args init'
        case last' of
            [] -> modifyIORef ref $ insert arg $ Const Nil
            x:xs -> modifyIORef ref $ insert arg $ Apply x xs

insert :: Ident -> Expr -> Env -> Env
insert var expr (Global m) = Global $ Map.insert var expr m
insert var expr (Extended m r) = Extended (Map.insert var expr m) r

union :: Map.Map Ident Expr -> Env -> Env
union m (Global m') = Global $ Map.union m m'
union m (Extended m' r) = Extended (Map.union m m') r

setVar :: MonadBase IO m => EnvRef -> Ident -> Expr -> m Expr
setVar ref var expr = readIORef ref >>= set
  where
    set (Global map') = set' map' $ throwIO $ Unbind var
    set (Extended map' ref') = set' map' $ setVar ref' var expr
    set' map' els
        | Map.member var map' = do
              modifyIORef ref $ insert var expr
              return expr
        | otherwise = els
