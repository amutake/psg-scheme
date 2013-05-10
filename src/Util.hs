{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.Traversable (Traversable, traverse)

import Types

splitIdents :: MonadBase IO m => List Value -> m (Ident, List Ident)
splitIdents vals = extractIdents vals >>= split
  where
    split (ProperList []) = throwIO $ Undefined "syntax error"
    split (ProperList (name:params)) = return (name, ProperList params)
    split (DottedList [] _) = throwIO $ Undefined "syntax error"
    split (DottedList (name:params) param) =
        return (name, DottedList params param)

extractIdents :: (MonadBase IO m, Traversable t) => t Value -> m (t Ident)
extractIdents = traverse extract
  where
    extract (Ident i) = return i
    extract s = throwIO $ Undefined $ show s
