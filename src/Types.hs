{-# LANGUAGE GeneralizedNewtypeDeriving
  , TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , DeriveDataTypeable
  #-}

module Types where

import Control.Applicative (Applicative)
import Control.Exception (Exception)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (StateT (..), MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control
    ( MonadBaseControl (..)
    , MonadTransControl (..)
    , ComposeSt
    , defaultLiftBaseWith
    , defaultRestoreM
    , defaultLiftWith
    , defaultRestoreT
    )
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Map (Map)
import Data.Typeable (Typeable)
import Text.PrettyPrint.ANSI.Leijen (Doc)

data Env
    = Global (Map Ident Value)
    | Extended (Map Ident Value) Env

data SchemeException
    = ParseError Doc
    | NotFunction Ident
    deriving (Show, Typeable)

instance Exception SchemeException

newtype SchemeT m a = SchemeT
    { runSchemeT :: StateT Env m a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState Env
    , MonadBase base
    , MonadThrow
    )

instance MonadTrans SchemeT where
    lift = SchemeT . lift

instance MonadTransControl SchemeT where
    newtype StT SchemeT a = StScheme { unStScheme :: StT (StateT Env) a }
    liftWith = defaultLiftWith SchemeT runSchemeT StScheme
    restoreT = defaultRestoreT SchemeT unStScheme

instance MonadBaseControl base m => MonadBaseControl base (SchemeT m) where
    newtype StM (SchemeT m) a = StMSchemeT { unStMSchemeT :: ComposeSt SchemeT m a }
    liftBaseWith = defaultLiftBaseWith StMSchemeT
    restoreM = defaultRestoreM unStMSchemeT

data Value
    = Bool Bool
    | Number Integer
    | String String
    | Nil
    | Pair Value Value
    | Ident Ident
    deriving (Eq)

instance Show Value where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (String s) = show s
    show Nil = "()"
    show (Pair car Nil) = "(" ++ show car ++ ")"
    show (Pair (Ident "quote") (Pair cdr Nil)) = "'" ++ show cdr
    show (Pair car cdr) = "(" ++ showCar car ++ showCdr cdr
      where
        showCar = show
        showCdr (Pair ca Nil) = " " ++ showCar ca ++ ")"
        showCdr (Pair ca cd) = " " ++ showCar ca ++ showCdr cd
        showCdr v = " . " ++ show v ++ ")"
    show (Ident i) = i

type Ident = String

isProperList :: Value -> Bool
isProperList (Pair _ Nil) = True
isProperList (Pair _ cdr@(Pair _ _)) = isProperList cdr
isProperList (Pair _ _) = False
isProperList _ = False

isDottedList :: Value -> Bool
isDottedList (Pair _ Nil) = False
isDottedList (Pair _ cdr@(Pair _ _)) = isDottedList cdr
isDottedList (Pair _ _) = True
isDottedList _ = False
