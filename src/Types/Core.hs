{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Core where

import Control.Applicative (Applicative)
import Control.Monad.Base (MonadBase)
import Control.Monad.Error (MonadError, ErrorT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State(StateT, MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)

import Types.Exception
import Types.Macro

newtype SchemeT m a = SchemeT
    { runSchemeT :: ErrorT SchemeException (StateT Macro m) a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState Macro
    , MonadBase base
    , MonadError SchemeException
    )

instance MonadTrans SchemeT where
    lift = SchemeT . lift . lift
