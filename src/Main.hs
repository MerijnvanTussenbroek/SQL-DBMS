module Main where

import Library.Database
import Terminal (terminalEntrance)
import Library.ParserCombinators
import Lexer
import Parser
import Folders.TableCreationFolder
import Folders.TableInsertionFolder
import Folders.TableDeletionFolder
import Folders.TableSelectionFolder
import AST 

main :: IO ()
main = do
    --terminalEntrance EmptyDB
    let path = "./dbdef.txt"
    input <- readFile path
    let Just a = (choiceParser parseCreateStatement input)
    --print a
    let (t, s) = creationFolder a
    --putStrLn s
    --putStrLn "\n--------------------------------\n"
    --print t
    --putStrLn "\n--------------------------------\n"
    input2 <- readFile "./dbfill.txt"
    let Just b = choiceParser parseTableInsert input2
    --print b
    let (x, y) = insertionFolder t b
    --putStrLn "\n--------------------------------\n"
    --print x
    --putStrLn "\n--------------------------------\n"
    --print y
    let z =     Delete "city" 
                (BinaryExpression EqualComp 
                (Literal (Variable "test")) 
                (Literal (Integer 5)
                ))
    --let (table, string) = deletionFolder x z
    --putStrLn "\n--------------------------------\n"
    --print table
    --putStrLn "\n--------------------------------\n"
    --putStrLn string
    let select = "SELECT test FROM city WHERE test = 5;"
    --let lexed = parse lexer select
    --print lexed
    let Just last = choiceParser parseTableSelection select
    print last
    let (last1, last2) = selectionFolder x last
    putStrLn "\n--------------------------------\n"
    print last1
    putStrLn "\n--------------------------------\n"
    putStrLn last2
    putStrLn "Quitting Program"

