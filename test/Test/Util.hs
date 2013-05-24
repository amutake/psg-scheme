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

pairTest :: Spec
pairTest = do
    describe "cons, car, cdr" $ do
        it "works" $ do
            testScheme (concat
                [ "(cons \"hello, \" \"world!\")"
                , "(cons 0 ())"
                , "(cons 1 (cons 10 100))"
                , "(cons 1 (cons 10 (cons 100 '())))"
                , "(cons #t (cons \"aa\" (cons 3 '())))"
                , "(car '(0))"
                , "(cdr '(0))"
                , "(car '((1 2 3) (4 5 6)))"
                , "(cdr '(1 2 3 . 4))"
                , "(cdr (cons 3 (cons 2 (cons 1 '()))))"
                ]) `shouldReturn`
                [ "(\"hello, \" . \"world!\")"
                , "(0)"
                , "(1 10 . 100)"
                , "(1 10 100)"
                , "(#t \"aa\" 3)"
                , "0"
                , "()"
                , "(1 2 3)"
                , "(2 3 . 4)"
                , "(2 1)"
                ]
