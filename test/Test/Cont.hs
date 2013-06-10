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

shiftResetTest :: Spec
shiftResetTest = do
    describe "shift/reset" $ do
        it "works with simple input" $ do
            testScheme "(reset (+ 1 (shift (lambda (k) (k 2))) 3))" `shouldReturn` ["6"]
            testScheme "(reset (+ 1 (shift (lambda (k) (+ 10 (k 2)))) 3))" `shouldReturn` ["16"]
            testScheme "(* 2 (reset (+ 1 (shift (lambda (k) (+ 10 (k 2)))) 3)))" `shouldReturn` ["32"]
        it "works with saving continuation" $ do
            testScheme (concat
                [ "(define cc ())"
                , "(* 2 (reset (+ 1 (shift (lambda (k) (set! cc k) (+ 10 (k 2)))) 3)))"
                , "(cc 1)"
                , "(cc 10)"
                , "(+ (cc 1) 100)"
                ]) `shouldReturn` ["cc", "32", "5", "14", "105"]
