{-# LANGUAGE FlexibleContexts #-}

module Conversion.Macro where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Error (throwError)
import Control.Monad.State (MonadState (..))
import Data.Map (Map)
import qualified Data.Map as M

import Types.Core
import Types.Exception
import Types.Macro
import Types.Syntax.After
import Types.Util

macro :: (Functor m, Monad m) => Expr -> SchemeT m Expr
macro c@(Const _) = return c
macro v@(Var _) = return v
macro (Define var e) = Define var <$> macro e
macro (DefineMacro args e) = DefineMacro args <$> macro e
macro (Lambda args e) = DefineMacro args <$> macro e
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
macro (End e) = End <$> macro e

conv :: Monad m => [Expr] -> MacroBody -> SchemeT m Expr
conv args (MacroBody args' body) = do
    pairs <- argPairs args' args
    return $ mapExpr pairs body

mapExpr :: Map Ident Expr -> Expr -> Expr
mapExpr _ (Const c) = Const c
mapExpr m (Var var) = maybe (Var var) id $ M.lookup var m
mapExpr m (Define var e) = Define var $ mapExpr m e
mapExpr m (DefineMacro args e) = DefineMacro args $ mapExpr m e
mapExpr m (Lambda args e) = Lambda args $ mapExpr m e
mapExpr m (Func args e ref) = Func args (mapExpr m e) ref
mapExpr m (Apply e es) = Apply (mapExpr m e) $ map (mapExpr m) es
mapExpr m (CallCC cc args e) = CallCC cc args $ mapExpr m e
mapExpr _ (Prim p) = Prim p
mapExpr _ (Quote e) = Quote e
mapExpr m (Begin es) = Begin $ map (mapExpr m) es
mapExpr m (Set var e) = Set var $ mapExpr m e
mapExpr m (If b t f) = If (mapExpr m b) (mapExpr m t) (mapExpr m f)
mapExpr _ Undefined = Undefined
mapExpr m (End e) = End $ mapExpr m e

argPairs :: Monad m => Args -> [Expr] -> SchemeT m (Map Ident Expr)
argPairs (Args (ProperList args)) exprs
    | length args == length exprs = return $ M.fromList $ zip args exprs
    | otherwise = throwError $ NumArgs "macro: not match number of args"
argPairs (Args (DottedList args arg)) exprs
    | length args > length exprs = throwError $ NumArgs "macro: not match number of args"
    | otherwise = do
        let (init', last') = splitAt (length args) exprs
            init'' = zip args init'
            last''
                | null last' = [(arg, Const Nil)]
                | otherwise = [(arg, Apply (head last') (tail last'))]
        return $ M.fromList $ init'' ++ last''
