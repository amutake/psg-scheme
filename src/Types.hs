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
    | Undefined Ident
    | Exit
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

runScheme :: Monad m => SchemeT m a -> Env -> m (a, Env)
runScheme = runStateT . runSchemeT

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
    show (ProperList xs) = "(" ++ unwords (map show xs) ++ ")"
    show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
    show (Ident i) = i

type Ident = String
