module Test.Cont where

import Test.Hspec

import Test.Core

runContTests :: IO ()
runContTests = hspec $ do
    callCCTest

callCCTest :: Spec
callCCTest = do
    describe "call/cc" $ do
        it "works with simple input" $ do
            testScheme "(call/cc (lambda (k) (k 1)))" `shouldReturn` ["1"]
            testScheme "(+ (call/cc (lambda (k) (k 5))) 4)" `shouldReturn` ["9"]
        it "words with saving continuation" $ do
            testScheme (concat
                [ "(define cc ())"
                , "cc"
                , "(+ (call/cc (lambda (k) (set! cc k) 5)) 4)"
                , "(cc 1)"
                , "(+ (cc 1) 100)"
                ]) `shouldReturn` ["cc", "()", "9", "5", "5"]
