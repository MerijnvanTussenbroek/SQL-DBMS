module AST where


--------------------- Tokens ---------------------

data Token =    EmptyToken
                | INSERT
                | INTO
                | CREATE
                | TABLE
                | CONSTRAINT

                | OpeningRoundBracket
                | ClosingRoundBracket
                | Comma


                | NameToken Name
                | IntToken Int
                | StringToken String
                | BooleanToken Bool






















-------------- Abstract Syntax Tree --------------


data Statement = TableCreation | TableInsertion | TableDeletion | TableSelection


data TableCreation = Create [VariableInitialization] [Constraints]

data VariableInitialization = Name VarType

type Name = String

data VarType =  INT
                | TEXT
                | VARCHAR Int
                | BOOL
                | CHAR Int

data Constraints = Constraint

data TableInsertion = InsertInto Name [Name] [Values]

data Values =   Integer Int
                | Text String
                | Boolean Bool
                | Varchar Int
                | Character Int

data TableDeletion = Delete

data TableSelection = Select

data AttributeSelection =   End Name
                            | NameDot Name AttributeSelection

data Expression = Expr

data Condition = Condi
