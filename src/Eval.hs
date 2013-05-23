{-# LANGUAGE FlexibleContexts, CPP #-}

module Eval where

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.State (MonadState (..))
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)
import qualified Data.Map as M

import Env
import Primitives
import Types.Core
import Types.Exception
import Types.Macro
import Types.Syntax.After
import Types.Util


import Control.Monad.IO.Class (MonadIO (..))

eval :: (MonadBase IO m, MonadIO m) => EnvRef -> Expr -> SchemeT m Expr
eval _ c@(Const _) = return c
eval ref (Var v) = lookupEnv ref v >>= eval ref
eval ref (Define v e) = eval ref e >>= define ref v
eval ref (DefineMacro args expr) = eval ref expr >>= putMacro args
eval ref (Lambda args body) = return $ Func args body ref
eval _ f@(Func _ _ _) = return f
eval ref (Apply f es) = do
    f' <- eval ref f
    es' <- mapM (eval ref) es
    apply f' es'
eval ref (CallCC cc args body) = do
    defines ref args [cc]
    eval ref body
eval _ p@(Prim _) = return p
eval _ (Quote e) = return e
eval ref (Begin es) = mapM (eval ref) (init es) >> eval ref (last es)
eval ref (Set v e) = eval ref e >>= setVar ref v
eval ref (If b t f) = do
    b' <- eval ref b
    case b' of
        Const (Bool True) -> eval ref t
        _ -> eval ref f
eval _ Undefined = return Undefined
eval ref (End e) = eval ref e >>= throwIO . Next

apply :: (MonadBase IO m, MonadIO m) => Expr -> [Expr] -> SchemeT m Expr
apply (Prim f) es = applyPrim f es
apply (Func args body closure) es = do
    ref <- newIORef $ Extended empty closure
    defines ref args es
    eval ref body
apply e _ = throwIO $ NotFunction $ show e

applyPrim :: MonadBase IO m => Prim -> [Expr] -> m Expr
applyPrim Add = primAdd
applyPrim Sub = primSub
applyPrim Mul = primMul
applyPrim Div = primDiv
applyPrim Equal = primEqual

putMacro :: (MonadBase IO m, MonadIO m) => Args -> Expr -> SchemeT m Expr
putMacro args expr = do
    (var, args') <- splitArgs args
    macro <- get
    put $ M.insert var (MacroBody args' expr) macro
#if DEBUG
    liftIO $ putStrLn $ show args
    liftIO $ putStrLn $ show expr
    m' <- get
    liftIO $ print m'
#endif
    return $ Var var

splitArgs :: MonadBase IO m => Args -> m (Ident, Args)
splitArgs (Args (ProperList [])) =
    throwIO $ SyntaxError "define-macro: not find identifier"
splitArgs (Args (ProperList (e:es))) = return (e, Args $ ProperList es)
splitArgs (Args (DottedList [] _)) =
    throwIO $ SyntaxError "define-macro: not find identifier"
splitArgs (Args (DottedList (e:es) r)) =
    return (e, Args $ DottedList es r)
