{-# LANGUAGE FlexibleContexts #-}

module Conversion.Macro where

import Control.Applicative ((<$>))
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.State (MonadState (..))
import Data.Map (Map)
import qualified Data.Map as M

import Types.Core
import Types.Exception
import Types.Macro
import Types.Syntax.Before
import Types.Util

macro :: MonadBase IO m => Expr -> SchemeT m Expr
macro c@(Const _) = return c
macro (Ident i) = return $ Ident i
macro (List l) = macroList l

macroList :: MonadBase IO m => List Expr -> SchemeT m Expr
macroList e@(ProperList ((Ident i):args)) = do
    m <- get
    case M.lookup i m of
        Just body -> conv args body
        Nothing -> List <$> return e
macroList e@(ProperList _) = List <$> return e
macroList e@(DottedList _ _) = List <$> return e

conv :: MonadBase IO m => [Expr] -> MacroBody -> SchemeT m Expr
conv args (MacroBody args' body) = do
    pairs <- argPairs args' args
    return $ mapExpr pairs body

mapExpr :: Map Ident Expr -> Expr -> Expr
mapExpr _ (Const c) = Const c
mapExpr m (Ident i) = case M.lookup i m of
    Just e -> e
    Nothing -> Ident i
mapExpr m (List (ProperList es)) =
    List $ ProperList $ map (mapExpr m) es
mapExpr m (List (DottedList es e)) =
    List $ DottedList (map (mapExpr m) es) $ mapExpr m e

argPairs :: MonadBase IO m => Args -> [Expr] -> m (Map Ident Expr)
argPairs (Args (ProperList args)) exprs
    | length args == length exprs = return $ M.fromList $ zip args exprs
    | otherwise = throwIO $ NumArgs "macro: not match number of args"
argPairs (Args (DottedList args arg)) exprs
    | length args > length exprs = throwIO $ NumArgs "macro: not match number of args"
    | otherwise = do
        let (init', last') = splitAt (length args) exprs
            init'' = zip args init'
            last'' = [(arg, List $ ProperList last')]
        return $ M.fromList $ init'' ++ last''
