module Main where

import Text.Trifecta (parseTest)

import Parser

main :: IO ()
main = do
    str <- getLine
    if null str
        then putStrLn "bye"
        else do
            parseTest parseValueList str
            main
