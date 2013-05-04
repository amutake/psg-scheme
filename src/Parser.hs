module Parser where

import Control.Applicative ((<|>), (<$>), (<*>), (*>), (<*))
import Control.Comonad (($>))
import Control.Monad.Trans.Resource (MonadThrow (..))
import Data.Char (toLower)
import Data.Monoid (mempty)
import Text.Trifecta hiding (parseString, doc)
import qualified Text.Trifecta as T

import Types

parse :: MonadThrow m => String -> SchemeT m Value
parse str = case T.parseString parseValue mempty str of
    Success val -> return val
    Failure doc -> monadThrow $ ParseError doc

parseValue :: Parser Value
parseValue = between spaces spaces $
    parseBool <|>
    parseNumber <|>
    parseString <|>
    parseProperList <|>
    parseDottedList <|>
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

parseProperList :: Parser Value
parseProperList = try . parens $
    ProperList <$> many parseValue

parseDottedList :: Parser Value
parseDottedList = try . parens $ flattenList <$> (DottedList <$>
    some parseValue <* symbol "." <*> parseValue)

flattenList :: Value -> Value
flattenList (DottedList xs x) = case flattenList x of
    ProperList ys -> ProperList $ map flattenList $ xs ++ ys
    DottedList ys y -> DottedList (map flattenList $ xs ++ ys) y
    y -> DottedList xs y
flattenList v = v

parseQuote :: Parser Value
parseQuote = ProperList <$>
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
