module Test.Macro where

import Test.Hspec

import Test.Core

runMacroTests :: IO ()
runMacroTests = hspec $ do
    letTest
    andTest
    letStarTest

letTest :: Spec
letTest = do
    describe "let" $ do
        it "works" $ do
            testScheme "(let ((x 1) (y 2)) (+ x y))" `shouldReturn` ["3"]

andTest :: Spec
andTest = do
    describe "and" $ do
        it "works" $ do
            testScheme "(and 1 2 3 4)" `shouldReturn` ["4"]
            testScheme "(and #f 1 2)" `shouldReturn` ["#f"]
            testScheme "(and 1 2 3 #f)" `shouldReturn` ["#f"]

letStarTest :: Spec
letStarTest = do
    describe "let*" $ do
        it "works" $ do
            testScheme "(let* ((x 1) (y x)) (+ x y))" `shouldReturn` ["2"]
