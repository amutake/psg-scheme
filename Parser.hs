module Parser where

import Control.Applicative ((<|>), (<$>))
import Text.Trifecta hiding (parseString)

import Types

parseValue :: Parser Value
parseValue = parseNumber <|> parseString <|> parseList

parseNumber :: Parser Value
parseNumber = Number <$> integer

parseString :: Parser Value
parseString = String <$> stringLiteral

parseList :: Parser Value
parseList = List <$> list parseValue

list :: Parser a -> Parser [a]
list parser = parens $ sepBy parser spaces
