{-# LANGUAGE DeriveDataTypeable #-}

module Types.Exception where

import Control.Exception (Exception)
import Control.Monad.Error.Class (Error)
import Data.Typeable (Typeable)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Types.Syntax

data SchemeException
    = ParseError Doc
    | NotFunction String
    | Unbind Ident
    | NumArgs String
    | TypeMismatch String
    | SyntaxError String
    | IOError IOError
    | Break Expr
    | Exit
    deriving (Typeable)

instance Exception SchemeException

instance Error SchemeException

instance Show SchemeException where
    show (ParseError doc) = "parse-error: " ++ show doc
    show (NotFunction s) = "not-function: " ++ s
    show (Unbind i) = "unbind: " ++ i
    show (NumArgs s) = "wrong number args: " ++ s
    show (TypeMismatch s) = "type-mismatch: expect: " ++ s
    show (SyntaxError s) = "syntax-error: " ++ s
    show (IOError e) = "io-error: " ++ show e
    show (Break e) = show e
    show Exit = "exit"
