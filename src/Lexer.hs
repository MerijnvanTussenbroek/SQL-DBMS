module Lexer where

import Library.Parsers
import Library.ParserCombinators
import Library.ElementaryParsers

import Control.Applicative

import AST

lexKeywords :: Parser Char Token
lexKeywords =   CREATE <$ token "CREATE"
                <|> TABLE <$ token "TABLE"
                <|> REFERENCES <$ token "REFERENCES"
                <|> PRIMARYKEY <$ token "PRIMARY KEY"
                <|> FOREIGNKEY <$ token "FOREIGN KEY"
                <|> CHECK <$ token "CHECK"
                <|> CONSTRAINT <$ token "CONSTRAINT"
                <|> INSERT <$ token "INSERT"
                <|> INTO <$ token "INTO"
                <|> DELETE <$ token "DELETE"
                <|> SELECT <$ token "SELECT"
                <|> VALUES <$ token "VALUES"
                <|> FROM <$ token "FROM"
                <|> WHERE <$ token "WHERE"
                <|> DISTINCT <$ token "DISTINCT"
                <|> ALL <$ token "ALL"

lexVarTypes :: Parser Char Token
lexVarTypes =   TokenINT <$ token "INT"
                <|> TokenBOOL <$ token "BOOL"
                <|> TokenTEXT <$ token "TEXT"
                <|> TokenVARCHAR <$ token "VARCHAR"
                <|> TokenCHAR <$ token "CHAR"

lexSymbols :: Parser Char Token
lexSymbols =    OpeningRoundBracket <$ symbol '('
                <|> ClosingRoundBracket <$ symbol ')'
                <|> Comma <$ symbol ','
                <|> Semicolon <$ symbol ';'

lexValues :: Parser Char Token
lexValues =     (BooleanToken True <$ token "TRUE"
                <|> BooleanToken False <$ token "FALSE"
                <|> IntToken <$> parseInteger)
                <<|> (((\_ s _ -> StringToken s) <$> symbol '\'' <*> greedy (satisfy (/= '\'')) <*> symbol '\'')
                <<|> NameToken <$> parseIdentifier )
                

lexWhiteSpace :: Parser Char Token
lexWhiteSpace = EmptyToken <$ spaceParser

operatorTokenList :: [(String, Operator)]
operatorTokenList = [("*", Mul), ("/", Div), ("%",Mod),("+",Add),("-",Min),("<=",LessThanOrEqual),("<",LessThan),(">=",GreaterThanOrEqual),(">",GreaterThan),("=",EqualComp),("!=",NotEqualComp), ("!", LogicalNot),("&&",LogicalAnd),("||",LogicalOr)]

lexOperators :: Parser Char Token
lexOperators = foldr (\(s, t) p -> (OperatorToken t <$ token s)  <|> p) failp operatorTokenList 


lexerList :: [Parser Char Token]
lexerList =     [
                    lexWhiteSpace
                    ,lexKeywords
                    ,lexVarTypes
                    ,lexOperators
                    ,lexSymbols
                    ,lexValues
                ]


fullLexer :: Parser Char Token
fullLexer = greedyChoice lexerList

lexer :: Parser Char [Token]
lexer = fmap (filter (/= EmptyToken)) (greedy fullLexer)
