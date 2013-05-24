module Main where

import Test.Cont
import Test.Util

main :: IO ()
main = do
    runUtilTests
    runContTests
