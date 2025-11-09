module Main where

import Library.ParserCombinators
import Lexer
import Parser
import Terminal

main :: IO ()
main = do
    -- terminalEntrance
    let filepath = "/home/alice/Documents/Haskell/SQL-DBMS/dbdef.txt"
    input <- readFile filepath
    putStrLn "Begin lexing"
    let x = parse lexer input
    print x
    --let (a, b) = head x
    --print a
    --let y = parse parseCreateStatement a
    --print y
    putStrLn "Quitting Program"
