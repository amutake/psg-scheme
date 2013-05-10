module Test.Util where

import Test.Hspec

import Types
import Util

runUtilTests :: IO ()
runUtilTests = hspec $ do
    extractIdentsTest
    splitIdentsTest

splitIdentsTest :: Spec
splitIdentsTest = do
    describe "splitIdents" $ do
        it "works when input is a proper list of identifier only" $ do
            splitIdents (
                ProperList [Ident "x", Ident "y", Ident "z"]
                ) `shouldReturn` ("x", ProperList ["y", "z"])
        it "works when input is a dotted list of identifier only" $ do
            splitIdents (
                DottedList [Ident "x", Ident "y"] (Ident "z")
                ) `shouldReturn` ("x", DottedList ["y"] "z")
        it "throw exception when input is a proper list of empty" $ do
            splitIdents (
                ProperList []
                ) `shouldThrow` anyException
        it "throw exception when input is a dotted list of empty" $ do
            splitIdents (
                DottedList [] (Ident "x")
                ) `shouldThrow` anyException

extractIdentsTest :: Spec
extractIdentsTest = do
    describe "extractIdents" $ do
        it "works when input is list of identifier only" $ do
            extractIdents (
                [Ident "x", Ident "y", Ident "z"]
                ) `shouldReturn` ["x", "y", "z"]
        it "works when input is proper list of identifier only" $ do
            extractIdents (
                ProperList [Ident "x", Ident "y", Ident "z"]
                ) `shouldReturn` ProperList ["x", "y", "z"]
        it "works when input is dotted list of identifier only" $ do
            extractIdents (
                DottedList [Ident "x", Ident "y"] (Ident "z")
                ) `shouldReturn` DottedList ["x", "y"] "z"
        it "works when input is empty list" $ do
            extractIdents (ProperList []) `shouldReturn` ProperList []
        it "works when input is empty proper list" $ do
            extractIdents (
                DottedList [] (Ident "z")
                ) `shouldReturn` DottedList [] "z"
        it "works when input is empty dotted list" $ do
            extractIdents [] `shouldReturn` []
        it "throw exception when input is a proper list of not only identifier" $ do
            splitIdents (
                ProperList [Ident "x", Number 1, Ident "z"]
                ) `shouldThrow` anyException
        it "throw exception when input is a dotted list of not only identifier" $ do
            splitIdents (
                DottedList [Ident "x", Number 1] (Ident "z")
                ) `shouldThrow` anyException
