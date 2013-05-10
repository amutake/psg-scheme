module Main where

import Test.Eval
import Test.Parser
import Test.Util

main :: IO ()
main = do
    runParserTests
    runEvalTests
    runUtilTests
