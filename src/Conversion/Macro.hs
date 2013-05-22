{-# LANGUAGE FlexibleContexts #-}

module Conversion.Macro where

import Control.Applicative ((<$>))
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.Map (Map)
import qualified Data.Map as M

import Types.Exception
import Types.Macro
import Types.Syntax.Before
import Types.Util

macro :: MonadBase IO m => Macro -> Expr -> m Expr
macro _ c@(Const _) = return c
macro _ (Ident i) = return $ Ident i
macro m (List l) = macroList m l

macroList :: MonadBase IO m => Macro -> List Expr -> m Expr
macroList m e@(ProperList ((Ident i):args)) = do
    case M.lookup i m of
        Just body -> conv args body
        Nothing -> List <$> return e
macroList _ e@(ProperList _) = List <$> return e
macroList _ e@(DottedList _ _) = List <$> return e

conv :: MonadBase IO m => [Expr] -> MacroBody -> m Expr
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
