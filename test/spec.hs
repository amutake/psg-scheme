module Main where

import Test.Cont
import Test.Macro
import Test.Util

main :: IO ()
main = do
    runUtilTests
    runContTests
    runMacroTests
