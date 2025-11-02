module AST where


--------------------- Tokens ---------------------

data Token =    CREATE
                | TABLE
                | PRIMARYKEY
                | REFERENCES
                | FOREIGNKEY                


                | OpeningRoundBracket
                | ClosingRoundBracket
                | Comma
                | Semicolon



                | CharToken Char
                | NameToken Name
                | IntToken Int
                | StringToken String
                | BooleanToken Bool






















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

data TableCreation = Create Name [ColumnDef] TableConstraint

-- column_def ::= column_name [ type_name ] ( column_constraint ) *

data ColumnDef = ColDef Name VarType ColumnConstraint

-- table_constraint ::=    [ CONSTRAINT name ]
--                        (
--                        ( PRIMARY KEY | UNIQUE column_name )
--                        CHECK '(' expr ')' |
--                        FOREIGN KEY '(' column_name ( ',' column_name ) * ')' foreign_key_clause )

data TableConstraint = TabConstraint (Maybe Name) (Maybe (Bool, Name)) [Expression] (Maybe ForeignKeyClause)

-- foreign_key_clause ::=   REFERENCES foreign_table
--                          [ '(' column_name ( ',' column_name ) * ')' ]

data ForeignKeyClause = ForeignKeyClause [Name] Name [Name]

-- column_constraint ::= [ CONSTRAINT name ]
--                        ( PRIMARY KEY |
--                        CHECK '(' expr ')' |
--                        foreign_key_clause )

data ColumnConstraint = ColConstraint (Maybe Bool) (Maybe Expression) (Maybe ForeignKeyClause)

data TableInsertion = InsertInto

data TableDeletion = Delete 

data TableSelection = Select

data Expression = Expr

data Condition = Condi


data VarType =  INT
                | TEXT
                | VARCHAR Int
                | BOOL
                | CHAR Int

data Values =   Integer Int
                | Text String
                | Boolean Bool
                | VarChar String
                | Character Char

type Name = String
