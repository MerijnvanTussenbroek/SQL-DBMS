module Library.Parsers where

import Data.Char

import Library.ParserCombinators
import Library.ElementaryParsers
import Control.Applicative

spaceParser :: Parser Char Char
spaceParser = satisfy isSpace

parseSymbol :: Parser Char Char
parseSymbol = satisfy isAlphaNum

digit :: Parser Char Int
digit = (\x -> read [x]) <$> satisfy isDigit

parseFirstLetter :: Parser Char Char
parseFirstLetter = satisfy isLower <|> satisfy isUpper <|> symbol '_'

natural :: Parser Char Int
natural = foldl (\a b -> 10 * a + b) 0 <$> greedy1 digit

parseInteger :: Parser Char Int
parseInteger = (\a b -> if a == '-' then -b else b)
    <$> (symbol '-' <|> succeed ' ')
    <*> natural

parseDouble :: Parser Char Double
parseDouble = (\a b _ d -> if a == '-' then -(f b d) else f b d ) 
    <$> (satisfy (== '-') <|> succeed ' ') 
    <*> natural 
    <*> satisfy (== '.') 
    <*> natural
    where
        f b d = fromIntegral b + (fromIntegral d / (10 ^ length (show d)))

choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) failp

chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr p q = output <$> many (middleparser <$> p <*> q) <*> p
  where
    middleparser :: a -> (a -> a -> a) -> (a -> a)
    middleparser x op = (x `op`)
    output :: [a -> a] -> a -> a
    output = flip (foldr ($))

chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl p q = output <$> p <*> many (middleparser <$> q <*> p)
    where
        output :: a -> [a -> a] -> a
        output= foldl (flip ($))
        middleparser :: (a -> a -> a) -> a -> (a -> a)
        middleparser op x = (`op` x)

