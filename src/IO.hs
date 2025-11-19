module IO where


import Parser
import Library.Database
import AST
import Control.Exception
import System.IO.Error


readSQLFromFile :: String -> IO (Maybe [Statements])
readSQLFromFile = readSomethingFromFile parseInput

readDBFromFile :: String -> IO (Maybe Database)
readDBFromFile = readSomethingFromFile parseDB

readSomethingFromFile :: (String -> Maybe s) -> String -> IO (Maybe s)
readSomethingFromFile f path = do
    input <- tryToRead path
    return (if null input then Nothing else f input)


tryToRead :: String -> IO String
tryToRead path = handle resolveIOException (readFile path)

saveToFile :: Database -> String -> IO String
saveToFile db path = undefined

resolveIOException :: IOException -> IO String
resolveIOException e    | isDoesNotExistError e = do
    print e
    putStrLn "File couldn't be found"
    return []
                        | isAlreadyInUseError e = do
    print e
    putStrLn "File already in use."
    return []
                        | otherwise = do
    print e
    putStrLn "Unhandled file exception"
    return []



