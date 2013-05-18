{-# LANGUAGE DeriveDataTypeable #-}

module Types.Exception where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Types.Util

data SchemeException
    = ParseError Doc
    | NotFunction String
    | Unbind Ident
    | NumArgs String
    | TypeMismatch String
    | SyntaxError String
    | Exit
    deriving (Show, Typeable)

instance Exception SchemeException