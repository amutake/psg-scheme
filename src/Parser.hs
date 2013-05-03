module Parser where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*), pure)
import Control.Comonad (($>))
import Data.Char (toLower)
import Text.Trifecta hiding (parseString)

import Types

parseValue :: Parser Value
parseValue = between spaces spaces $
    parseBool <|>
    parseNumber <|>
    parseString <|>
    parseNil <|>
    parsePair <|>
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

parseNil :: Parser Value
parseNil = try (parens whiteSpace) $> Nil

parsePair :: Parser Value
parsePair = try parseProperList <|> try parseDottedList

parseProperList :: Parser Value
parseProperList = symbol "(" *> pair
  where
    end = symbol ")" $> Nil
    pair = Pair <$> parseValue <*> (end <|> pair)

parseDottedList :: Parser Value
parseDottedList = symbol "(" *> pair
  where
    end = symbol "." *> parseValue <* symbol ")"
    pair = Pair <$> parseValue <*> (end <|> pair)

parseQuote :: Parser Value
parseQuote = Pair <$>
    (symbol "'" $> Ident "quote") <*>
    (Pair <$> parseValue <*> pure Nil)

parseIdent :: Parser Value
parseIdent = Ident . map toLower <$> (
    notFollowedBy (Ident <$> symbol "." <|> parseNumber) *>
    token (some identChar)
    )

identChar :: Parser Char
identChar = alphaNum <|> oneOf "!$%&*+-./<=>?@^_"
