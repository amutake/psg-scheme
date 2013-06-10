{-# LANGUAGE GeneralizedNewtypeDeriving
  , ConstraintKinds
  , TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

module Types.Core where

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Error (MonadError, ErrorT (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State(StateT (..), MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control
    ( MonadBaseControl (..)
    , MonadTransControl (..)
    , ComposeSt
    , defaultLiftBaseWith
    , defaultRestoreM
    )

import Types.Exception
import Types.Syntax

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

instance MonadTransControl SchemeT where
    newtype StT SchemeT a = StScheme { unStScheme :: (Either SchemeException a, Macro) }
    liftWith f = SchemeT . ErrorT $ liftM return $ StateT $ \s ->
        liftM (\x -> (x, s))
              (f $ \t -> liftM StScheme (runStateT (runErrorT (runSchemeT t)) s))
    restoreT = SchemeT . ErrorT . StateT . const . liftM unStScheme

instance MonadBaseControl base m => MonadBaseControl base (SchemeT m) where
    newtype StM (SchemeT m) a = StMSchemeT { unStMSchemeT :: ComposeSt SchemeT m a }
    liftBaseWith = defaultLiftBaseWith StMSchemeT
    restoreM = defaultRestoreM unStMSchemeT

type MonadScheme m = (Functor m, Monad m, MonadIO m, MonadBase IO m)
