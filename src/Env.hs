{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.IORef.Lifted (readIORef)
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
