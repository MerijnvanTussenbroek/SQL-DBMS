{-# LANGUAGE InstanceSigs #-}

module Library.ParserCombinators where

import Control.Applicative
import Control.Monad

newtype Parser input output = Parser { parse :: [input] -> [(output, [input])] }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser s) = Parser $ \input -> [ (f x,xs) | (x,xs) <- s input ]

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure p = Parser $ \input -> [(p, input)]
    (<*>) :: Parser s (a ->b) -> Parser s a -> Parser s b
    (Parser p) <*> (Parser q) = Parser $ \input -> [ (x z, zs) | (x,xs) <- p input, (z,zs) <- q xs]

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ \_ -> []
    (<|>) :: Parser s a -> Parser s a -> Parser s a
    (Parser p) <|> (Parser q) = Parser $ \input -> p input ++ q input

instance Monad (Parser s) where
    return :: a -> Parser s a
    return = pure
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    p >>= f = Parser $ \input -> [ (y,ys) | (x,xs) <- parse p input, (y,ys) <- parse (f x) xs]

instance MonadPlus (Parser s) where
    mzero :: Parser s a
    mzero = empty
    mplus :: Parser s a -> Parser s a -> Parser s a
    mplus = (<|>)

infixr 3 <<|>
(<<|>) :: Parser s a -> Parser s a -> Parser s a
p <<|> q = Parser $ \input -> let r = parse p input in if null r then parse q input else r
