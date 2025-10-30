# SQL-DBMS

This is a small terminal SQLite that can take command inputs, read from and write to files, 
and perform all the basic SQLite commands.

This project is divided into mutliple files and 1 subfolder

src ---- Main.hs
      |- AST.hs
      |- Algebra.hs
      |- Parser.hs
      |- Lexer.hs
      |- IO.hs
      |- Terminal.hs
      |---- library
        |- ElementaryParser.hs
        |- ParserCombinators.hs
        |- Parsers.hs

The parsers are again based off of the uu-tc parser library.
