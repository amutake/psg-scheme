module Test.Parser where

import Data.Monoid
import Test.Hspec
import Text.Trifecta

import Parser hiding (parseString)
import Types

runParserTests :: IO ()
runParserTests = hspec $ do
    parseValueTest

parseValueTest :: Spec
parseValueTest = do
    describe "parseValue" $ do
        it "parse (1 2 3)" $ do
            "(1 2 3)" `shouldBeParsed`
                List [Number 1, Number 2, Number 3]
        it "parse ((1 2) (3) 4)" $ do
            "((1 2) (3) 4)" `shouldBeParsed`
                List [List [Number 1, Number 2], List [Number 3], Number 4]
        it "parse (#t #f #t)" $ do
            "(#t #f #t)" `shouldBeParsed`
                List [Bool True, Bool False, Bool True]
  where
    shouldBeParsed str expect =
        parseString parseValue mempty str `shouldSatisfy`
            check expect
    check value result = case result of
        Success v -> v == value
        Failure _ -> False
