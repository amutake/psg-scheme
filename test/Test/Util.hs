module Test.Util where

import Test.Hspec

import Types.Syntax.After
import Types.Util

import Test.Core

runUtilTests :: IO ()
runUtilTests = hspec $ do
    loadTest

loadTest :: Spec
loadTest = do
    describe "load" $ do
        it "works" $ do
            testScheme "(load \"preload/util.scm\") (list 1 2 3)" `shouldReturn`
                [ Const $ Bool True
                , Apply (Const $ Number 1) [Const $ Number 2, Const $ Number 3]
                ]
