{-# LANGUAGE FlexibleContexts, CPP #-}

module Eval where

import Control.Monad.Base (MonadBase)
import Control.Monad.Error (throwError)
#ifdef DEBUG
import Control.Monad.IO.Class (MonadIO (..))
#endif
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

#ifdef DEBUG
eval :: (MonadIO m, MonadBase IO m) => EnvRef -> Expr -> SchemeT m Expr
#else
eval :: MonadBase IO m => EnvRef -> Expr -> SchemeT m Expr
#endif
eval _ c@(Const _) = return c
eval ref (Var v) = lookupEnv ref v
eval ref (Define v e) = eval ref e >>= define ref v
eval ref (DefineMacro args expr) = putMacro args expr ref
eval ref (Lambda args body) = return $ Func args body ref
eval _ f@(Func _ _ _) = return f
eval ref (Apply f es) = do
    f' <- eval ref f
    es' <- mapM (eval ref) es
#ifdef DEBUG
    liftIO $ putStrLn $ ("  apply-before: " ++) $ show $ Apply f es
    liftIO $ putStrLn $ ("  apply-after: " ++) $ show $ Apply f' es'
#endif
    case f' of
        End -> throwError . Next . last $ es'
        _ -> apply f' es'
eval ref (CallCC cc args body) = do
    cc' <- eval ref cc
    defines ref args [cc']
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
eval _ End = return End

#ifdef DEBUG
apply :: (MonadBase IO m, MonadIO m) => Expr -> [Expr] -> SchemeT m Expr
#else
apply :: MonadBase IO m => Expr -> [Expr] -> SchemeT m Expr
#endif
apply (Prim f) es = applyPrim f es
apply (Func args body closure) es = do
    ref <- newIORef $ Extended empty closure
    defines ref args es
    eval ref body
apply e _ = throwError $ NotFunction $ show e

applyPrim :: Monad m => Prim -> [Expr] -> SchemeT m Expr
applyPrim Add = primAdd
applyPrim Sub = primSub
applyPrim Mul = primMul
applyPrim Div = primDiv
applyPrim Equal = primEqual

putMacro :: MonadBase IO m => Args -> Expr -> EnvRef -> SchemeT m Expr
putMacro args expr ref = do
    (var, args') <- splitArgs args
    macro <- get
    put $ M.insert var (MacroBody args' expr ref) macro
    return $ Var var

splitArgs :: MonadBase IO m => Args -> SchemeT m (Ident, Args)
splitArgs (Args (ProperList [])) =
    throwError $ SyntaxError "define-macro: not find identifier"
splitArgs (Args (ProperList (e:es))) = return (e, Args $ ProperList es)
splitArgs (Args (DottedList [] _)) =
    throwError $ SyntaxError "define-macro: not find identifier"
splitArgs (Args (DottedList (e:es) r)) =
    return (e, Args $ DottedList es r)
