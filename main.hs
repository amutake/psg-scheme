module Main where

import Control.Applicative ((<|>), (<$>))
import Text.Trifecta hiding (parseString)

main :: IO ()
main = do
    str <- getLine
    if null str
        then putStrLn "bye"
        else do
            parseTest parseValueList str
            main

parseValueList :: Parser Value
parseValueList = List <$> parseList parseValue

parseList :: Parser a -> Parser [a]
parseList parser = parens $ sepBy parser spaces

data Value
    = Number Integer
    | String String
    | List [Value]
    deriving (Show)

parseValue :: Parser Value
parseValue = parseNumber <|> parseString

parseString :: Parser Value
parseString = String <$> stringLiteral

parseNumber :: Parser Value
parseNumber = Number <$> integer
