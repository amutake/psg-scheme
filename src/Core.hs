{-# LANGUAGE FlexibleContexts, CPP #-}

module Core where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Exception (try)
#ifdef DEBUG
#else
import Control.Monad ((>=>))
#endif
import Control.Monad.Base (MonadBase)
import Control.Monad.Error (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (..))
import Data.IORef.Lifted (newIORef)
import Data.Map (empty)
import qualified Data.Map as M

import Env
import Primitives
import Conversion.Normalize (normalize)
import Conversion.CPS (cps)
import Parser (parse)
import Types.Core (SchemeT (..))
import Types.Exception
import Types.Syntax.After
import Types.Macro
import Types.Util

#ifdef DEBUG
scheme :: (Functor m, Monad m, MonadIO m, MonadBase IO m) => EnvRef -> String -> SchemeT m [Expr]
scheme ref s = do
    bes <- parse s
    liftIO $ putStrLn $ "parse: " ++ show bes
    mapM (\be -> do
        ae <- normalize be
        liftIO $ putStrLn $ "normalize: " ++ show ae
        me <- macro ae
        liftIO $ putStrLn $ "macro: " ++ show me
        let ce = cps me
        liftIO $ putStrLn $ "cps: " ++ show ce
        eval ref ce
        ) bes
#else
scheme :: (Functor m, Monad m, MonadBase IO m, MonadIO m) => EnvRef -> String -> SchemeT m [Expr]
scheme ref = parse >=> mapM (normalize >=> macro >=> eval ref . cps)
#endif

----------------
-- eval
----------------

eval :: (Functor m, Monad m, MonadIO m, MonadBase IO m) => EnvRef -> Expr -> SchemeT m Expr
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
        End -> return $ last es'
        _ -> apply f' es'
eval _ (Dot _ _) = throwError $ SyntaxError "dotted list"
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
eval ref (Load e) = eval ref e >>= load ref
eval _ Undefined = return Undefined
eval _ End = return End

apply :: (MonadBase IO m, MonadIO m) => Expr -> [Expr] -> SchemeT m Expr
apply (Prim f) es = applyPrim f es
apply (Func args body closure) es = do
    ref <- newIORef $ Extended empty closure
    defines ref args es
    eval ref body
apply e _ = throwError $ NotFunction $ show e

applyPrim :: (Functor m, Monad m, MonadIO m, MonadBase IO m) => Prim -> [Expr] -> SchemeT m Expr
applyPrim Add = primAdd
applyPrim Sub = primSub
applyPrim Mul = primMul
applyPrim Div = primDiv
applyPrim Equal = primEqual
applyPrim Eqv = primEqv
applyPrim Car = primCar

load :: (Functor m, Monad m, MonadIO m, MonadBase IO m) => EnvRef -> Expr -> SchemeT m Expr
load ref e = do
    path <- extractString e
    result <- liftIO $ try $ readFile path
    case result of
        Left err -> throwError $ IOError err
        Right str -> (scheme ref str >> return (Const $ Bool True)) `catchError`
            (\err -> liftIO (print err) >> return (Const $ Bool False))

extractString :: Monad m => Expr -> SchemeT m String
extractString (Const (String str)) = return str
extractString _ = throwError $ TypeMismatch "String"

putMacro :: MonadBase IO m => Args -> Expr -> EnvRef -> SchemeT m Expr
putMacro args expr ref = do
    (var, args') <- splitArgs args
    mac <- get
    put $ M.insert var (MacroBody args' expr ref) mac
    return $ Var var

splitArgs :: MonadBase IO m => Args -> SchemeT m (Ident, Args)
splitArgs (Args (ProperList [])) =
    throwError $ SyntaxError "define-macro: not find identifier"
splitArgs (Args (ProperList (e:es))) = return (e, Args $ ProperList es)
splitArgs (Args (DottedList [] _)) =
    throwError $ SyntaxError "define-macro: not find identifier"
splitArgs (Args (DottedList (e:es) r)) =
    return (e, Args $ DottedList es r)

----------------
-- macro
----------------

macro :: (MonadIO m, MonadBase IO m) => Expr -> SchemeT m Expr
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
macro (Dot es e) = Dot <$> mapM macro es <*> macro e
macro (CallCC cc args e) = CallCC <$> macro cc <*> pure args <*> macro e
macro p@(Prim _) = return p
macro (Quote e) = Quote <$> macro e
macro (Begin es) = Begin <$> mapM macro es
macro (Set var e) = Set var <$> macro e
macro (If b t f) = If <$> macro b <*> macro t <*> macro f
macro (Load e) = Load <$> macro e
macro Undefined = return Undefined
macro End = return End

conv :: (MonadIO m, MonadBase IO m) => [Expr] -> MacroBody -> SchemeT m Expr
conv args (MacroBody args' body ref) = do
    ref' <- newIORef $ Extended empty ref
    defines ref' args' args
    eval ref' body
