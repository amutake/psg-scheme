module Test.Parser where

import Data.Monoid
import Test.Hspec
import Text.Trifecta

import Parser hiding (parseString)
import Types

runParserTests :: IO ()
runParserTests = hspec $ do
    parseBoolTest
    parseNumberTest
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
        it "cannot parse +-1" $ do
            parseNumber `cannotParse` "+-1"

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
    parseString parser mempty str `shouldSatisfy` check expect
  where
    check value result = case result of
        Success v -> v == value
        Failure _ -> False

cannotParse :: (Eq a, Show a) => Parser a -> String -> Expectation
cannotParse parser str =
    parseString parser mempty str `shouldSatisfy` check
  where
    check result = case result of
        Success _ -> False
        Failure _ -> True
