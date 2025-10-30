module Main where

import Parser

main :: IO ()
main = do
    let x = testFunc
    print x
    putStrLn "Hello, Haskell!"
