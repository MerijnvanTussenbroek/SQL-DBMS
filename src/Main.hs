module Main where

import Library.ParserCombinators
import Lexer
import Parser
import AST
import Terminal
import Data.Maybe

main :: IO ()
main = do
    -- terminalEntrance
    let filepath = "./dbdef.txt"
    input <- readFile filepath
    let testInput = "DELETE FROM test WHERE (test < 5, test2 + 4 != 5)"
    let y = parseInput input
    let z = if isNothing y then "Program couldn't be parsed" else show (fromJust y)
    putStrLn z
    putStrLn "Quitting Program"


parseInput :: String -> Maybe [Statements]
parseInput input = returnValue
    where
        x = parse lexer input
        y = f x
        (a,_) = head x
        z = if y then parse parser a else []
        c = f z
        (d,_) = head z
        returnValue = if c then Just d else Nothing
        f [] = False
        f _ = True

