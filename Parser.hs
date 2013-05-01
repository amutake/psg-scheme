module Parser where

import Control.Applicative ((<|>), (<$>))
import Text.Trifecta hiding (parseString)

import Types

parseValue :: Parser Value
parseValue = parseBool <|> parseNumber <|> parseString <|> parseList

parseBool :: Parser Value
parseBool = Bool <$> parseTrue <|> Bool <$> parseFalse
  where
    parseTrue = string "#t" >> return True
    parseFalse = string "#f" >> return False

parseNumber :: Parser Value
parseNumber = Number <$> integer

parseString :: Parser Value
parseString = String <$> stringLiteral

parseList :: Parser Value
parseList = List <$> list parseValue

list :: Parser a -> Parser [a]
list parser = parens $ sepBy parser spaces
