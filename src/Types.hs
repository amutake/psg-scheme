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
import Data.Map (Map)
import Data.Typeable (Typeable)
import Text.PrettyPrint.ANSI.Leijen (Doc)

data Env
    = Global (Map Ident Value)
    | Extended (Map Ident Value) Env

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
    | ProperList [Value]
    | DottedList [Value] Value
    | Ident Ident
    deriving (Eq)

instance Show Value where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (String s) = show s
    show (ProperList ((Ident "quote") : [x])) = "'" ++ show x
    show (ProperList xs) = "(" ++ unwords (map show xs) ++ ")"
    show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
    show (Ident i) = i

type Ident = String
