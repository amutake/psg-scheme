{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*))
import Control.Comonad (($>))
import Control.Monad.Error (MonadError (..))
import Data.Char (toLower)
import Data.Monoid (mempty)
import Text.Trifecta hiding (parseString, doc)
import qualified Text.Trifecta as T

import Types.Core
import Types.Exception
import Types.Syntax.Before
import Types.Util

parse :: Monad m => String -> SchemeT m [Expr]
parse str = case T.parseString parseExprs mempty str of
    Success expr -> return expr
    Failure doc -> throwError $ ParseError doc

parseExprs :: Parser [Expr]
parseExprs = many parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = between blank blank $
    parseConst <|>
    parseList <|>
    parseQuote <|>
    parseQuasiQuote <|>
    parseUnquoteSplicing <|>
    parseUnquote <|>
    parseIdent

parseConst :: Parser Expr
parseConst = Const <$> (parseBool <|> parseNumber <|> parseString <|> parseUndefined)

parseBool :: Parser Const
parseBool = parseTrue <|> parseFalse
  where
    parseTrue = symbol "#t" $> Bool True
    parseFalse = symbol "#f" $> Bool False

parseNumber :: Parser Const
parseNumber = try . token $ Number <$>
    integer' <* notFollowedBy identChar

parseString :: Parser Const
parseString = try $ String <$> stringLiteral

parseUndefined :: Parser Const
parseUndefined = try $ symbol "undefined" $> Undefined

parseList :: Parser Expr
parseList = List <$> (parseProperList <|> parseDottedList)

parseProperList :: Parser (List Expr)
parseProperList = try . parens $
    ProperList <$> many parseExpr

parseDottedList :: Parser (List Expr)
parseDottedList = try . parens $ DottedList <$>
    some parseExpr <* symbol "." <*> parseExpr

parseQuote :: Parser Expr
parseQuote = try $ List . ProperList <$>
    (two <$> (symbol "'" $> Ident "quote") <*> parseExpr)
  where
    two x y = [x, y]

parseQuasiQuote :: Parser Expr
parseQuasiQuote = try $ List . ProperList <$>
    (two <$> (symbol "`" $> Ident "quasiquote") <*> parseExpr)
  where
    two x y = [x, y]

parseUnquote :: Parser Expr
parseUnquote = try $ List . ProperList <$>
    (two <$> (symbol "," $> Ident "unquote") <*> parseExpr)
  where
    two x y = [x, y]

parseUnquoteSplicing :: Parser Expr
parseUnquoteSplicing = try $ List . ProperList <$>
    (two <$> (symbol ",@" $> Ident "unquote-splicing") <*> parseExpr)
  where
    two x y = [x, y]

parseIdent :: Parser Expr
parseIdent = Ident . map toLower <$> (
    notFollowedBy (Ident <$> symbol "." <|> Const <$> parseNumber) *>
    token (some identChar)
    )

identChar :: Parser Char
identChar = alphaNum <|> oneOf "!$%&*+-./<=>?@^_"

blank :: Parser ()
blank = spaces <|> skipMany newline <|> skipMany tab
