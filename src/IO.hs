module IO where


import Parser
import AST
import Control.Exception
import System.IO.Error


readSQLFromFile :: String -> IO (Maybe [Statements])
readSQLFromFile = readSomethingFromFile parseInput

readDBFromFile :: String -> IO (Maybe String)
readDBFromFile = readSomethingFromFile Just

readSomethingFromFile :: (String -> Maybe s) -> String -> IO (Maybe s)
readSomethingFromFile f path = do
    input <- tryToRead path
    return (if null input then Nothing else f input)


tryToRead :: String -> IO String
tryToRead path = handle resolveIOException (readFile path)



resolveIOException :: IOException -> IO String
resolveIOException e    | isDoesNotExistError e = do
    print e
    putStrLn "File couldn't be found"
    return []
                        | otherwise = do
    print e
    putStrLn "Unhandled file exception"
    return []


saveToFile :: String -> IO String
saveToFile path = undefined
