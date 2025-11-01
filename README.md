# SQL-DBMS

## This is a small terminal SQLite that can take command inputs, read from and write to files, 
and perform all the basic SQLite commands.

### This project is divided into mutliple files and 1 subfolder

src
├── Main
├── AST
├── Algebra
├── Parser
├── Lexer
├── IO
├── Terminal
└── Library
    ├── ElementaryParsers
    ├── ParserCombinators
    └── Parsers

### The parsers are again based off of the uu-tc parser library.

We will begin with implementing the following commands, before moving to the more complex ones:

    expressions
    table creation
    constraints
    insertion into tables
    deletion from tables
    selection from tables
