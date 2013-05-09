{-# LANGUAGE GeneralizedNewtypeDeriving
  , TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , DeriveDataTypeable
  #-}

module Types where

import Control.Exception (Exception)
import Control.Monad.Trans.Identity
import Data.IORef.Lifted (IORef)
import Data.Map (Map)
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
    | Ident Ident
    deriving (Eq)

instance Show Value where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (String s) = show s
    show (List (ProperList ((Ident "quote") : [x]))) = "'" ++ show x
    show (List l) = show l
    show (Ident i) = i

type Ident = String

data List a
    = ProperList [a]
    | DottedList [a] a
    deriving (Eq)

instance Show a => Show (List a) where
    show (ProperList xs) = "(" ++ unwords (map show xs) ++ ")"
    show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
