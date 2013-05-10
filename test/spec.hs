module Main where

import Test.Eval
import Test.Parser

main :: IO ()
main = do
    runParserTests
    runEvalTests
