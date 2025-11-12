module Main where

import Library.ParserCombinators
import Lexer
import Parser
import Terminal

main :: IO ()
main = do
    -- terminalEntrance
    let filepath = "./q1.txt"
    input <- readFile filepath
    let testInput = "DELETE FROM test WHERE (test < 5, test2 + 4 != 5)"
    putStrLn "Begin lexing"
    let x = parse lexer input
    print x
    let (a, b) = head x
    --print a
    let y = parse parseTableSelection a
    let (c,d) = head y
    print c
    putStrLn "Quitting Program"
