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
import Types.Syntax

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
eval ref (DefineMacro v e) = eval ref e >>= putMacro v
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
eval ref (QuasiQuote e) = evalQuasiQuote ref e
eval ref (Unquote e) = eval ref e
eval ref (UnquoteSplicing e) = eval ref e
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

evalQuasiQuote :: (Functor m, Monad m, MonadIO m, MonadBase IO m) => EnvRef -> Expr -> SchemeT m Expr
evalQuasiQuote _ c@(Const _) = return c
evalQuasiQuote _ v@(Var _) = return v
evalQuasiQuote ref (Define v e) = Define v <$> evalQuasiQuote ref e
evalQuasiQuote ref (DefineMacro v e) = DefineMacro v <$> evalQuasiQuote ref e
evalQuasiQuote ref (Lambda args body) = Lambda args <$> evalQuasiQuote ref body
evalQuasiQuote _ f@(Func _ _ _) = return f
evalQuasiQuote ref (Apply f es) = case f of
    UnquoteSplicing e -> do
        e' <- eval ref e
        case e' of
            Apply f' es' -> Apply f' <$> ((es' ++) <$> go ref es)
            _ -> throwError $ SyntaxError ",@expr must be evaluated to list"
    _ -> Apply <$> evalQuasiQuote ref f <*> go ref es
  where
    go _ [] = return []
    go gref ((UnquoteSplicing ge):ges) = do
        ge' <- eval gref ge
        case ge' of
            Apply gf' ges' -> ((gf' : ges') ++) <$> go gref ges
            _ -> (ge' :) <$> go gref ges
    go gref (ge:ges) = (:) <$> evalQuasiQuote gref ge <*> go gref ges
evalQuasiQuote _ d@(Dot _ _) = return d
evalQuasiQuote ref (CallCC cc args body) = CallCC <$> evalQuasiQuote ref cc <*> pure args <*> evalQuasiQuote ref body
evalQuasiQuote _ p@(Prim _) = return p
evalQuasiQuote _ q@(Quote _) = return q
evalQuasiQuote ref (QuasiQuote e) = evalQuasiQuote ref e
evalQuasiQuote ref (Unquote e) = eval ref e
evalQuasiQuote _ (UnquoteSplicing _) = throwError $ SyntaxError "this is bug! evalQuasiQuote"
evalQuasiQuote ref (Begin es) = Begin <$> mapM (evalQuasiQuote ref) es
evalQuasiQuote ref (Set v e) = Set v <$> evalQuasiQuote ref e
evalQuasiQuote ref (If b t f) = If <$> evalQuasiQuote ref b <*> evalQuasiQuote ref t <*> evalQuasiQuote ref f
evalQuasiQuote ref (Load e) = Load <$> evalQuasiQuote ref e
evalQuasiQuote _ Undefined = return Undefined
evalQuasiQuote _ End = return End

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
applyPrim Cdr = primCdr
applyPrim Cons = primCons

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

putMacro :: MonadBase IO m => Ident -> Expr -> SchemeT m Expr
putMacro var (Func args expr ref) = do
    mac <- get
    put $ M.insert var (MacroBody args expr ref) mac
    return $ Var var
putMacro _ _ = throwError $ SyntaxError "define-macro"

----------------
-- macro
----------------

macro :: (MonadIO m, MonadBase IO m) => Expr -> SchemeT m Expr
macro c@(Const _) = return c
macro v@(Var _) = return v
macro (Define var e) = Define var <$> macro e
macro (DefineMacro v e) = DefineMacro v <$> macro e
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
macro (QuasiQuote e) = QuasiQuote <$> macro e
macro (Unquote e) = Unquote <$> macro e
macro (UnquoteSplicing e) = UnquoteSplicing <$> macro e
macro (Begin es) = Begin <$> mapM macro es
macro (Set var e) = Set var <$> macro e
macro (If b t f) = If <$> macro b <*> macro t <*> macro f
macro (Load e) = Load <$> macro e
macro Undefined = return Undefined
macro End = return End

conv :: (MonadIO m, MonadBase IO m) => [Expr] -> MacroBody -> SchemeT m Expr
conv args (MacroBody args' body ref) = do
    ref' <- newIORef $ Extended empty ref
    defines ref' args' $ End : args
    eval ref' body
