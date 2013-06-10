{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Util where

import Control.Monad.Error (throwError)
import Data.Traversable (Traversable (..))

import Types.Core
import Types.Exception
import Types.Syntax

nil :: List a
nil = ProperList []

cons :: a -> List a -> List a
cons x (ProperList xs) = ProperList (x:xs)
cons x (DottedList xs x') = DottedList (x:xs) x'

extractIdents :: (MonadScheme m, Traversable t) => t Expr -> SchemeT m (t Ident)
extractIdents = traverse extract
  where
    extract (Ident i) = return i
    extract s = throwError $ SyntaxError $ "extractIdents: " ++ show s

two :: a -> a -> [a]
two a b = [a, b]

idE :: EnvRef -> Ident -> Expr
idE ref v = Func (ProperList [v]) (Ident v) ref
