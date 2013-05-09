{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*))
import Control.Comonad (($>))
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase)
import Data.Char (toLower)
import Data.Monoid (mempty)
import Text.Trifecta hiding (parseString, doc)
import qualified Text.Trifecta as T

import Types

parse :: MonadBase IO m => String -> SchemeT m Value
parse str = case T.parseString parseValue mempty str of
    Success val -> return val
    Failure doc -> throwIO $ ParseError doc

parseValue :: Parser Value
parseValue = between spaces spaces $
    parseBool <|>
    parseNumber <|>
    parseString <|>
    parseList <|>
    parseQuote <|>
    parseIdent

parseBool :: Parser Value
parseBool = parseTrue <|> parseFalse
  where
    parseTrue = symbol "#t" $> Bool True
    parseFalse = symbol "#f" $> Bool False

parseNumber :: Parser Value
parseNumber = try . token $ Number <$>
    integer' <* notFollowedBy identChar

parseString :: Parser Value
parseString = try $ String <$> stringLiteral

parseList :: Parser Value
parseList = flattenList . List <$> (parseProperList <|> parseDottedList)

parseProperList :: Parser (List Value)
parseProperList = try . parens $
    ProperList <$> many parseValue

parseDottedList :: Parser (List Value)
parseDottedList = try . parens $ DottedList <$>
    some parseValue <* symbol "." <*> parseValue

flattenList :: Value -> Value
flattenList (List (DottedList xs x)) = case x of
    List (ProperList ys) -> List $ ProperList $ map flattenList $ xs ++ ys
    List (DottedList ys y) -> List $ DottedList (map flattenList $ xs ++ ys) y
    y -> List $ DottedList xs y
flattenList v = v

parseQuote :: Parser Value
parseQuote = List . ProperList <$>
    (two <$> (symbol "'" $> Ident "quote") <*> parseValue)
  where
    two x y = [x, y]

parseIdent :: Parser Value
parseIdent = Ident . map toLower <$> (
    notFollowedBy (Ident <$> symbol "." <|> parseNumber) *>
    token (some identChar)
    )

identChar :: Parser Char
identChar = alphaNum <|> oneOf "!$%&*+-./<=>?@^_"
