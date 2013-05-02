module Parser where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*))
import Control.Comonad (($>))
import Text.Trifecta hiding (parseString)

import Types

parseValue :: Parser Value
parseValue =
    parseBool <|>
    parseNumber <|>
    parseString <|>
    parseNil <|>
    parsePair <|>
    parseIdent

parseBool :: Parser Value
parseBool = parseTrue <|> parseFalse
  where
    parseTrue = symbol "#t" $> Bool True
    parseFalse = symbol "#f" $> Bool False

parseNumber :: Parser Value
parseNumber = Number <$> integer

parseString :: Parser Value
parseString = String <$> stringLiteral

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

parseIdent :: Parser Value
parseIdent = Ident <$> some (alphaNum <|> oneOf "!$%&*+-./<=>?@^_")
