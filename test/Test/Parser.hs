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
    parseProperListTest
    parseDottedListTest
    parseQuoteTest
    parseIdentTest
    parseValueTest
    parseShowTest

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

parseProperListTest :: Spec
parseProperListTest = do
    describe "parseProperList" $ do
        it "can parse ()" $ do
            parseProperList `canParse` "()" $ ProperList []
        it "can parse (  )" $ do
            parseProperList `canParse` "(  )" $ ProperList []
        it "can parse (())" $ do
            parseProperList `canParse` "(())" $ ProperList [ProperList []]
        it "can parse (() () ())" $ do
            parseProperList `canParse` "(() () ())" $
                ProperList [ProperList [], ProperList [], ProperList[]]
        it "can parse (((()) ()) ())" $ do
            parseProperList `canParse` "(((()) ()) ())" $ do
                ProperList [ProperList[ProperList [ProperList[]], ProperList[]], ProperList[]]
        it "can't parse ((())" $ do
            parseProperList `cannotParse` "((())"
        it "can parse (()))" $ do
            parseProperList `canParse` "(()))" $ ProperList [ProperList[]]
        it "can parse (() . ())" $ do
            parseProperList `cannotParse` "(() . ())"

parseDottedListTest :: Spec
parseDottedListTest = do
    describe "parseDottedList" $ do
        it "can parse (() . ())" $ do
            parseDottedList `canParse` "(() . ())" $ DottedList [ProperList[]] (ProperList [])
        it "can parse (() () () . ())" $ do
            parseDottedList `canParse` "(() () () . ())" $
                DottedList [ProperList [], ProperList [], ProperList []] (ProperList [])
        it "can parse (((() . ()) . (() . ())) . (() . ()))" $ do
            parseDottedList `canParse` "(((() . ()) . (() . ())) . (() . ()))" $
                DottedList [DottedList [DottedList [ProperList []] (ProperList [])] (DottedList [ProperList []] (ProperList []))] (DottedList [ProperList []] (ProperList []))

parseQuoteTest :: Spec
parseQuoteTest = do
    describe "parseQuote" $ do
        it "can parse '(1 2)" $ do
            parseQuote `canParse` "'(1 2)" $
                ProperList [Ident "quote", ProperList [Number 1, Number 2]]
        it "can parse '''a" $ do
            parseQuote `canParse` "'''a" $ do
                ProperList [Ident "quote", ProperList [Ident "quote", ProperList [Ident "quote", Ident "a"]]]
        it "can parse '('a 'b)"$ do
            parseQuote `canParse` "'('a 'b)" $ do
                ProperList [Ident "quote", ProperList [ProperList [Ident "quote", Ident "a"], ProperList [Ident "quote", Ident "b"]]]

parseIdentTest :: Spec
parseIdentTest = do
    describe "parseIdent" $ do
        it "can parse amkkun" $ do
            parseIdent `canParse` "amkkun" $ Ident "amkkun"
        it "can parse AmKkUn" $ do
            parseIdent `canParse` "AmKkUn" $ Ident "amkkun"
        it "can parse amkkun123" $ do
            parseIdent `canParse` "amkkun123" $ Ident "amkkun123"
        it "can parse 123amkkun" $ do
            parseIdent `canParse` "123amkkun" $ Ident "123amkkun"
        it "can't parse 123" $ do
            parseIdent `cannotParse` "123"
        it "can parse !$%&*+-./<=>?@^_" $ do
            parseIdent `canParse` "!$%&*+-./<=>?@^_" $
                Ident "!$%&*+-./<=>?@^_"
        it "can't parse ." $ do
            parseIdent `cannotParse` "."

parseValueTest :: Spec
parseValueTest = do
    describe "parseValue" $ do
        it "can parse (1 2 3)" $ do
            parseValue `canParse` "(1 2 3)" $
                ProperList [Number 1, Number 2, Number 3]
        it "can parse ((1 2) (3) 4)" $ do
            parseValue `canParse` "((1 2) (3) 4)" $
                ProperList [ProperList [Number 1, Number 2], ProperList [Number 3], Number 4]
        it "can parse (#t #f #t)" $ do
            parseValue `canParse` "(#t #f #t)" $
                ProperList [Bool True, Bool False, Bool True]
        it "can parse (define (S x y z) (x z (y z)))" $
            parseValue `canParse` "(define (S x y z) (x z (y z)))" $
                ProperList [Ident "define", ProperList [Ident "s", Ident "x", Ident "y", Ident "z"], ProperList [Ident "x", Ident "z", ProperList [Ident "y", Ident "z"]]]

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

parseShowTest :: Spec
parseShowTest = do
    describe "parse and show" $ do
        it "parse (define (S x y z) (x z (y z))), then show (define (s x y z) (x z (y z)))" $ do
            "(define (S x y z) (x z (y z)))" `shouldBeParsed`
                "(define (s x y z) (x z (y z)))"
        it "parse  ( \"a\"  m ( k k ) \"un\" 123  #f ) , then show (\"a\" m (k k) \"un\" 123 #f)" $ do
            " ( \"a\"  m ( k k ) \"un\" 123  #f ) " `shouldBeParsed`
                "(\"a\" m (k k) \"un\" 123 #f)"
        it "parse (1 . (2 . (3 . ()))), then show (1 2 3)" $ do
            "(1 . (2 . (3 . ())))" `shouldBeParsed` "(1 2 3)"
        it "parse ((1 2 . 3) (1 2) () (1) . (1 2 . ())), then show ((1 2 . 3) (1 2) () (1) . (1 2))" $ do
            "((1 2 . 3) (1 2) () (1) . (1 2 . ()))" `shouldBeParsed`
                "((1 2 . 3) (1 2) () (1) 1 2)"
        it "parse ''('a '(b 'c)), then show ''('a '(b 'c))" $ do
            "''('a '(b 'c))" `shouldBeParsed` "''('a '(b 'c))"
  where
    shouldBeParsed str expect =
        T.parseString parseValue mempty str `shouldSatisfy`
            check expect
    check expect result = case result of
        Success v -> show v == expect
        Failure _ -> False
