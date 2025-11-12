module AST where


--------------------- Tokens ---------------------

data Token =    EmptyToken
                |CREATE
                | TABLE
                | PRIMARYKEY
                | REFERENCES
                | FOREIGNKEY                
                | CHECK
                | CONSTRAINT
                | INSERT
                | INTO
                | DELETE
                | SELECT
                | VALUES
                | FROM
                | WHERE

                | TokenINT
                | TokenTEXT
                | TokenBOOL
                | TokenVARCHAR
                | TokenCHAR

                | OpeningRoundBracket
                | ClosingRoundBracket
                | Comma
                | Semicolon

                | OperatorToken Operator

                | CharToken Char
                | IntToken Int
                | StringToken String
                | BooleanToken Bool
                | NameToken Name
            deriving(Eq, Show)







 












-------------- Abstract Syntax Tree --------------

data Statement = TableCreation | TableInsertion | TableDeletion | TableSelection





-- create_table_stmt ::=    CREATE TABLE
--                          table_name
--                          ( 
--                              '(' 
--                                  column_def ( ',' column_def ) * 
--                                  ( ',' table_constraint ) * 
--                              ')' 
--                          )

data TableCreation = Create Name [ColumnDef] [TableConstraint]
    deriving Show

-- column_def ::= column_name [ type_name ] ( column_constraint ) *

data ColumnDef = ColDef Name VarType (Maybe ColumnConstraint)
    deriving Show
-- table_constraint ::=    [ CONSTRAINT name ]
--                        (
--                        ( PRIMARY KEY column_name )
--                        CHECK '(' expr ')' |
--                        FOREIGN KEY '(' column_name ( ',' column_name ) * ')' foreign_key_clause )

data TableConstraint =  TabPrimKey (Maybe Name) Name
                        | TabCheck (Maybe Name) Expression
                        | TabForeKey (Maybe Name) ForeignKeyClause1 
    deriving Show
-- foreign_key_clause ::=   REFERENCES foreign_table
--                          [ '(' column_name ( ',' column_name ) * ')' ]

data ForeignKeyClause1 = ForeignKeyClause1 Name Name Name
    deriving Show

data ForeignKeyClause2 = ForeignKeyClause2 Name [Name]
    deriving Show

-- column_constraint ::= [ CONSTRAINT name ]
--                        ( PRIMARY KEY |
--                        CHECK '(' expr ')' |
--                        foreign_key_clause )

data ColumnConstraint = ColConstraint (Maybe Name) Bool (Maybe Expression) (Maybe ForeignKeyClause2)
    deriving Show

-- insert_stmt ::=  [ with_clause ] 
--                  ( INSERT) 
--                  INTO table_name [ '(' column_name ( ',' column_name ) * ')' ] 
--                  ( VALUES '(' expr ( ',' expr ) * ')'  

data TableInsertion = InsertInto Name [Name] [Expression]
    deriving Show




-- delete_stmt_limited ::=  DELETE FROM qualified_table_name
--                          [ WHERE expr ]


data TableDeletion = Delete Name [Expression]
    deriving Show



-- select_stmt ::=  ( SELECT [ DISTINCT | ALL ] result_column ( ',' result_column ) *
--                  [ FROM ( table_or_subquery ( ',' table_or_subquery ) * | join_clause ) ]
--                  [ WHERE expr ]

data TableSelection = Select (Maybe Bool) [Name] [JoinExpression] Expression

data JoinExpression = Join Name JoinOperator Name (Maybe Expression)

data JoinOperator = NATURAL | LEFT | INNER | CROSS



data Expression =   BinaryExpression Operator Expression Expression
                    | UnaryExpression Expression Operator
                    | TableRowSelection (Maybe Name) Name
                    | Literal Values
                    | CASTAS Expression VarType
    deriving Show
data Operator = Operator
                | Mul
                | Div
                | Mod
                | Add
                | Min

                | LessThan
                | LessThanOrEqual
                | GreaterThan
                | GreaterThanOrEqual
                | EqualComp
                | NotEqualComp

                | LogicalNot
                | LogicalAnd
                | LogicalOr

                | Assignment
    deriving(Eq, Show)


data VarType =  INT
                | TEXT
                | VARCHAR Int
                | BOOL
                | CHAR Int
    deriving Show

data Values =   Integer Int
                | Text String
                | Boolean Bool
                | VarChar String
                | Character Char
                | Variable String
                
    deriving Show

type Name = String
