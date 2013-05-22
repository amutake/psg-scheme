module Types.Util where

import Prelude hiding (foldr)
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (Foldable (..))
import Data.Traversable (Traversable (..))

type Ident = String

data Const
    = Bool Bool
    | Number Integer
    | String String
    | Nil
    deriving (Eq)

instance Show Const where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (String s) = show s
    show Nil = "()"

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

newtype Args = Args (List Ident) deriving (Eq)

instance Show Args where
    show (Args (ProperList xs)) = "(" ++ unwords xs ++ ")"
    show (Args (DottedList xs x)) = "(" ++ unwords xs ++ " . " ++ x ++ ")"
