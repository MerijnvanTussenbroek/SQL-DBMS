module Parser where

import Library.Parsers
import Library.ParserCombinators
import Library.ElementaryParsers

import Control.Applicative 

import AST

parseCreateStatement :: Parser Token TableCreation
parseCreateStatement = do
                    symbol CREATE
                    symbol TABLE
                    name <- parseName 
                    symbol OpeningRoundBracket
                    vars <- option [] (listOf parseColumnDef (symbol Comma))
                    tableCons <- option [] (flip const <$> symbol Comma <*> listOf parseTableConstraint (symbol Comma)) 
                    symbol ClosingRoundBracket
                    symbol Semicolon
                    return (Create name vars tableCons)

parseColumnDef :: Parser Token ColumnDef
parseColumnDef = ColDef <$> parseName <*> parseVarType <*> succeed Nothing

-- option Nothing (Just <$> parseColumnConstraint)



parseColumnConstraint :: Parser Token ColumnConstraint
parseColumnConstraint = ColConstraint 
                        <$> option Nothing (Just <$> parseString)
                        <*> option False (True <$ symbol PRIMARYKEY)
                        <*> option Nothing (Just <$> parseExpression)
                        <*> option Nothing (Just <$> parseForeignKey)

parseTableConstraint :: Parser Token TableConstraint
parseTableConstraint =  TabPrimKey <$> option Nothing (Just <$> parseName) <*> parseName


parseVarType :: Parser Token VarType
parseVarType =  (INT <$ symbol TokenINT)
                <|> (TEXT <$ symbol TokenTEXT)
                <|> (BOOL <$ symbol TokenBOOL)
                <|> ((\_ _ i _ -> VARCHAR i) <$> symbol TokenVARCHAR <*> symbol OpeningRoundBracket <*> parseInt <*> symbol  ClosingRoundBracket)
                <|> ((\_ _ i _ -> CHAR i) <$> symbol TokenCHAR <*> symbol OpeningRoundBracket <*> parseInt <*> symbol ClosingRoundBracket)

parseForeignKey :: Parser Token ForeignKeyClause
parseForeignKey = undefined

parseExpression :: Parser Token Expression
parseExpression = undefined

parseString :: Parser Token String
parseString = anySymbol >>= f
    where
        f (StringToken s) = return s
        f _ = failp

parseInt :: Parser Token Int
parseInt = anySymbol >>= f
    where
        f (IntToken i) = return i
        f _ = failp

parseBool :: Parser Token Bool
parseBool = anySymbol >>= f
    where
        f (BooleanToken b) = return b
        f _ = failp

parseChar :: Parser Token Char
parseChar = anySymbol >>= f
    where
        f (CharToken c) = return c
        f _ = failp

parseName :: Parser Token Name
parseName = anySymbol >>= f
    where
        f (NameToken n) = return n
        f _ = failp

parseOperator :: Parser Token Operator
parseOperator = anySymbol >>= f
    where
        f (OperatorToken op) = return op
        f _ = failp
