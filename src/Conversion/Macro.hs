{-# LANGUAGE FlexibleContexts, CPP #-}

module Conversion.Macro where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Base (MonadBase)
import Control.Monad.State (MonadState (..))
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)
import qualified Data.Map as M

import Env
import Eval
import Types.Core
import Types.Macro
import Types.Syntax.After

#ifdef DEBUG
import Control.Monad.IO.Class (MonadIO)

macro :: (MonadIO m, MonadBase IO m) => Expr -> SchemeT m Expr
#else
macro :: MonadBase IO m => Expr -> SchemeT m Expr
#endif
macro c@(Const _) = return c
macro v@(Var _) = return v
macro (Define var e) = Define var <$> macro e
macro (DefineMacro args e) = DefineMacro args <$> macro e
macro (Lambda args e) = Lambda args <$> macro e
macro (Func args e ref) = Func args <$> macro e <*> pure ref
macro a@(Apply (Var v) es) = do
    m <- get
    maybe (return a) (conv es) $ M.lookup v m
macro (Apply e es) = Apply <$> macro e <*> mapM macro es
macro (CallCC cc args e) = CallCC <$> macro cc <*> pure args <*> macro e
macro p@(Prim _) = return p
macro (Quote e) = Quote <$> macro e
macro (Begin es) = Begin <$> mapM macro es
macro (Set var e) = Set var <$> macro e
macro (If b t f) = If <$> macro b <*> macro t <*> macro f
macro Undefined = return Undefined
macro End = return End

#ifdef DEBUG
conv :: (MonadIO m, MonadBase IO m) => [Expr] -> MacroBody -> SchemeT m Expr
#else
conv :: MonadBase IO m => [Expr] -> MacroBody -> SchemeT m Expr
#endif
conv args (MacroBody args' body ref) = do
    ref' <- newIORef $ Extended empty ref
    defines ref' args' args
    eval ref' body
