module Test.Parser where

import Data.Monoid
import Test.Hspec
import Text.Trifecta (Result (..), Parser)
import qualified Text.Trifecta as T

import Parser
import Types

runParserTests :: IO ()
runParserTests = hspec $ do
    parseBoolTest
    parseNumberTest
    parseStringTest
    parseListTest
    parseValueTest

parseBoolTest :: Spec
parseBoolTest = do
    describe "parseBool" $ do
        it "can parse #t" $ do
            parseBool `canParse` "#t" $ Bool True
        it "can parse #f" $ do
            parseBool `canParse` "#f" $ Bool False
        it "can't parse ##" $ do
            parseBool `cannotParse` "##"
        it "can parse #ft" $ do
            parseBool `canParse` "#ft" $ Bool False

parseNumberTest :: Spec
parseNumberTest = do
    describe "parseNumber" $ do
        it "can parse 1" $ do
            parseNumber `canParse` "1" $ Number 1
        it "can parse 12345678901234567890" $ do
            parseNumber `canParse` "12345678901234567890" $
                Number 12345678901234567890
        it "can parse +1" $ do
            parseNumber `canParse` "+1" $ Number 1
        it "can parse -1" $ do
            parseNumber `canParse` "-1" $ Number (-1)
        it "can parse 001" $ do
            parseNumber `canParse` "001" $ Number 1
        it "can parse +001" $ do
            parseNumber `canParse` "+001" $ Number 1
        it "can't parse +-1" $ do
            parseNumber `cannotParse` "+-1"

parseStringTest :: Spec
parseStringTest = do
    describe "parseString" $ do
        it "can parse \"amkkun\"" $ do
            parseString `canParse` "\"amkkun\"" $ String "amkkun"
        it "can parse \"My name is amkkun\"" $ do
            parseString `canParse` "\"My name is amkkun\"" $
                String "My name is amkkun"
        it "can parse \"\\\\\"" $ do
            parseString `canParse` "\"\\\\\"" $ String "\\"
        it "can parse \"\\\"\"" $ do
            parseString `canParse` "\"\\\"\"" $ String "\""
        it "can parse \"\\n\"" $ do
            parseString `canParse` "\"\\n\"" $ String "\n"
        it "can parse \"\\n\\f\\b\\r\\t\\'\\\"\\\\\"" $ do
            parseString `canParse` "\"\\n\\f\\b\\r\\t\\'\\\"\\\\\"" $
                String "\n\f\b\r\t\'\"\\"
        it "can't parse abc" $ do
            parseString `cannotParse` "abc"

parseListTest :: Spec
parseListTest = do
    describe "parseList" $ do
        it "can parse ()" $ do
            parseList `canParse` "()" $ List []
        it "can parse (())" $ do
            parseList `canParse` "(())" $ List [List []]
        it "can parse (() () ())" $ do
            parseList `canParse` "(() () ())" $
                List [List [], List [], List []]
        it "can parse (((()) ()) ())" $ do
            parseList `canParse` "(((()) ()) ())" $ do
                List [List [List [List []], List []], List []]
        it "can't parse ((())" $ do
            parseList `cannotParse` "((())"
        it "can parse (()))" $ do
            parseList `canParse` "(()))" $
                List [List []]

parseValueTest :: Spec
parseValueTest = do
    describe "parseValue" $ do
        it "can parse (1 2 3)" $ do
            parseValue `canParse` "(1 2 3)" $
                List [Number 1, Number 2, Number 3]
        it "can parse ((1 2) (3) 4)" $ do
            parseValue `canParse` "((1 2) (3) 4)" $
                List [List [Number 1, Number 2], List [Number 3], Number 4]
        it "can parse (#t #f #t)" $ do
            parseValue `canParse` "(#t #f #t)" $
                List [Bool True, Bool False, Bool True]

canParse :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
canParse parser str expect =
    T.parseString parser mempty str `shouldSatisfy` check expect
  where
    check value result = case result of
        Success v -> v == value
        Failure _ -> False

cannotParse :: (Eq a, Show a) => Parser a -> String -> Expectation
cannotParse parser str =
    T.parseString parser mempty str `shouldSatisfy` check
  where
    check result = case result of
        Success _ -> False
        Failure _ -> True
