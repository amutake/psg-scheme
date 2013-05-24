module Test.Util where

import Test.Hspec

import Test.Core

runUtilTests :: IO ()
runUtilTests = hspec $ do
    loadTest

loadTest :: Spec
loadTest = do
    describe "load" $ do
        it "works" $ do
            testScheme "(load \"lib/util.scm\") (list 1 2 3)" `shouldReturn`
                [ "#t"
                , "(1 2 3)"
                ]
