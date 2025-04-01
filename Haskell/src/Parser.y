{
module Parser where

import AbstractSyntax
}

%name parser
%tokentype { Token }

%token
  x { Token }

%%

Program : { Program }

{

happyError _ = error "parse error"

}