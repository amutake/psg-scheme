module Main where

import Data.Monoid
import Text.Trifecta (parseString, Result (..))

import Eval
import Parser (parseValue)

main :: IO ()
main = do
    str <- getLine
    if null str
        then putStrLn "bye"
        else do
            case parseString parseValue mempty str of
                Success v -> print $ eval v
                Failure d -> print d
            main
