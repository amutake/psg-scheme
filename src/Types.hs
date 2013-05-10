{-# LANGUAGE DeriveDataTypeable
  , FlexibleContexts
  , RankNTypes
  #-}

module Types where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Identity
import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import Data.IORef.Lifted (IORef)
import Data.Map (Map)
import Data.Traversable (Traversable (..))
import Data.Typeable (Typeable)
import Text.PrettyPrint.ANSI.Leijen (Doc)

data Env
    = Global (Map Ident Value)
    | Extended (Map Ident Value) EnvRef

type EnvRef = IORef Env

data SchemeException
    = ParseError Doc
    | NotFunction Ident
    | Undefined Ident
    | TypeMismatch String
    | Exit
    deriving (Show, Typeable)

instance Exception SchemeException

type SchemeT = IdentityT

runSchemeT :: Monad m => SchemeT m a -> m a
runSchemeT = runIdentityT

data Value
    = Bool Bool
    | Number Integer
    | String String
    | List (List Value)
    | Func Func
    | Ident Ident
    deriving (Eq)

instance Show Value where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (String s) = show s
    show (List (ProperList ((Ident "quote") : [x]))) = "'" ++ show x
    show (List l) = show l
    show (Func f) = show f
    show (Ident i) = i

type Ident = String

data List a
    = ProperList [a]
    | DottedList [a] a
    deriving (Eq)

instance Show a => Show (List a) where
    show (ProperList xs) = "(" ++ unwords (map show xs) ++ ")"
    show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"

instance Functor List where
    fmap f (ProperList xs) = ProperList (fmap f xs)
    fmap f (DottedList xs x) = DottedList (fmap f xs) (f x)

instance Foldable List where
    foldr f z (ProperList xs) = foldr f z xs
    foldr f z (DottedList xs x) = foldr f z (xs ++ [x])

instance Traversable List where
    traverse f (ProperList xs) = ProperList <$> traverse f xs
    traverse f (DottedList xs x) = DottedList <$> traverse f xs <*> f x

data Func
    = Primitive (MonadBase IO m => [Value] -> m Value)
    | Lambda (List Ident) [Value] EnvRef

instance Eq Func where
    (Primitive _) == (Primitive _) = True
    (Primitive _) == (Lambda _ _ _) = False
    (Lambda _ _ _) == (Primitive _) = False
    (Lambda ids vals ref) == (Lambda ids' vals' ref') =
        and [ids == ids', vals == vals', ref == ref']

instance Show Func where
    show (Primitive _) = "<primitive function>"
    show (Lambda params body _) = "(lambda " ++ show params ++ " " ++ unwords (map show body) ++ ")"
