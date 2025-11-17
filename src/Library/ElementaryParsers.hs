module Library.ElementaryParsers where

import Library.ParserCombinators

anySymbol :: Parser s s
anySymbol = Parser $ \input -> case input of    
                                        (x:xs) -> [(x,xs)]
                                        [] -> []

failp :: Parser s a
failp = Parser $ \x -> []

satisfy :: (s -> Bool) -> Parser s s
satisfy f = Parser $ \input -> case input of
                                        (x:xs)  | f x -> [(x,xs)]
                                                | otherwise -> []
                                        [] -> []

symbol :: Eq s => s -> Parser s s
symbol x = satisfy (== x)

token :: Eq s => [s] -> Parser s [s]
token t = Parser $ \input -> case input of
                                    alg  | (take l alg) == t -> [(t,drop l alg)]
                                                | otherwise -> []
    where
        l = length t

succeed :: a -> Parser s a
succeed a = Parser $ \x -> [(a,x)]

eof :: Parser s ()
eof = Parser $ \input -> if null input then [( (), input)] else []

greedy :: Parser s a -> Parser s [a]
greedy p = (:) <$> p <*> greedy p <<|> succeed []

greedy1 :: Parser s a -> Parser s [a]
greedy1 p = (:) <$> p <*> greedy p

look :: Parser s [s]
look = Parser $ \input -> [(input, input)]

