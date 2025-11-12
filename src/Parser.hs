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
                    tableCons <- option [] (greedy (symbol Comma *> parseTableConstraint)) 
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
                        <*> option Nothing (Just <$> parseForeignKey2)

parseTableConstraint :: Parser Token TableConstraint
parseTableConstraint =  ((\_ a b -> TabForeKey a b) <$> symbol CONSTRAINT <*> parseMaybe parseName <*> parseForeignKey1)
                        <|> ((\_ a _ b -> TabPrimKey a b) <$> symbol CONSTRAINT <*> parseMaybe parseName <*> symbol PRIMARYKEY <*> parseBrackets parseName)
                        <|> ((\_ a _ b -> TabCheck a b) <$> symbol CONSTRAINT <*> parseMaybe parseName <*> symbol CHECK <*> parseBrackets parseExpression) 


parseVarType :: Parser Token VarType
parseVarType =  (INT <$ symbol TokenINT)
                <|> (TEXT <$ symbol TokenTEXT)
                <|> (BOOL <$ symbol TokenBOOL)
                <|> ((\_ i -> VARCHAR i) <$> symbol TokenVARCHAR <*> parseBrackets parseInt)
                <|> ((\_ _ i _ -> CHAR i) <$> symbol TokenCHAR <*> symbol OpeningRoundBracket <*> parseInt <*> symbol ClosingRoundBracket)

parseForeignKey1 :: Parser Token ForeignKeyClause1
parseForeignKey1 = (\_ a _ b c -> ForeignKeyClause1 a b c)<$> symbol FOREIGNKEY <*> parseBrackets parseName <*> symbol REFERENCES <*> parseName <*> parseBrackets parseName

parseForeignKey2 :: Parser Token ForeignKeyClause2
parseForeignKey2 = undefined



rightAssociative :: [[Operator]]
rightAssociative =  [
                        [LogicalOr, LogicalAnd],
                        [EqualComp, NotEqualComp],
                        [GreaterThan, GreaterThanOrEqual, LessThan, LessThanOrEqual],
                        [Add, Min],
                        [Mul, Div, Mod]
                    ]

leftAssociative :: [Operator]
leftAssociative =   [
                        Assignment
                    ]

leftGen :: [Operator] -> Parser Token Expression -> Parser Token Expression
leftGen ops p = chainr p (choice (map (\op -> helpFunc <$> symbol (OperatorToken op)) ops))

rightGen :: [Operator] -> Parser Token Expression -> Parser Token Expression
rightGen ops p = chainl p (choice (map (\op -> helpFunc <$> symbol (OperatorToken op)) ops))

helpFunc :: Token -> (Expression -> Expression -> Expression)
helpFunc (OperatorToken op) = BinaryExpression op
helpFunc _ = undefined

parseBinaryExpression :: Parser Token Expression
parseBinaryExpression = leftGen leftAssociative (foldr rightGen pAfter rightAssociative)

pAfter :: Parser Token Expression
pAfter = pLiteral <|> parseBrackets parseExpression

pLiteral :: Parser Token Expression
pLiteral = Literal <$>  (   (Integer <$> parseInt)
                            <|> (Text <$> parseString)
                            <|> (Boolean <$> parseBool)
                            <|> (Character <$> parseChar)
                            <|> (Variable <$> parseName)
                        )

parseExpression :: Parser Token Expression
parseExpression =   parseBinaryExpression
                    <|> pAfter



parseBrackets :: Parser Token a -> Parser Token a
parseBrackets p = (\_ a _ -> a) <$> symbol OpeningRoundBracket <*> p <*> symbol ClosingRoundBracket

parseMaybe :: Parser Token a -> Parser Token (Maybe a)
parseMaybe p = option Nothing (Just <$> p)

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
