module Parser where

import Control.Applicative ((<|>), (<$>))
import Text.Trifecta hiding (parseString)

import Types

parseValueList :: Parser Value
parseValueList = List <$> parseList parseValue

parseList :: Parser a -> Parser [a]
parseList parser = parens $ sepBy parser spaces

parseValue :: Parser Value
parseValue = parseNumber <|> parseString

parseString :: Parser Value
parseString = String <$> stringLiteral

parseNumber :: Parser Value
parseNumber = Number <$> integer
