{-# LANGUAGE GeneralizedNewtypeDeriving
  , TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

module Types.Core where

import Control.Applicative (Applicative)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State(StateT, MonadState)
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

import Types.Macro

newtype SchemeT m a = SchemeT
    { runSchemeT :: StateT Macro m a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState Macro
    , MonadBase base
    )

instance MonadTrans SchemeT where
    lift = SchemeT . lift

instance MonadTransControl SchemeT where
    newtype StT SchemeT a = StScheme { unStScheme :: StT (StateT Macro) a }
    liftWith = defaultLiftWith SchemeT runSchemeT StScheme
    restoreT = defaultRestoreT SchemeT unStScheme

instance MonadBaseControl base m => MonadBaseControl base (SchemeT m) where
    newtype StM (SchemeT m) a = StMSchemeT { unStMSchemeT :: ComposeSt SchemeT m a }
    liftBaseWith = defaultLiftBaseWith StMSchemeT
    restoreM = defaultRestoreM unStMSchemeT
