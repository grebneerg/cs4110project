%{
open Ast
open Lexing
%}

%token <int> INT
%token <char> CHAR
%token <string> VAR
%token <bool> BOOL
%token UNIT
%token LPAREN RPAREN LCURLY RCURLY COMMA COLON EQUALS
%token TRUE FALSE NOTEQUALS LESS LESSEQ GREATER GREATEREQ NOT AND OR PLUS MINUS MUL
%token LET IN
%token FUNCTION ARROW
%token IF
%token THEN
%token ELSE
%token EOF

%type <Ast.value> value
%type <Ast.binop> binop
%type <Ast.unop> unop
%type <Ast.expr> expr
%type <Ast.program> program


%start program

%%

unop : NOT                              { Not }

binop : EQUALS                          { Eq }
      | NOTEQUALS                       { Neq }
      | LESS                            { Lt }
      | LESSEQ                          { Leq }
      | GREATER                         { Gt }
      | GREATEREQ                       { Geq }
      | AND                             { And }
      | OR                              { Or }
      | PLUS                            { Add }
      | MINUS                           { Sub }
      | MUL                             { Mul }

expr : LET VAR EQUALS expr IN expr      { Let ($2, $4, $6) }
     | unop expr                        { UnOp ($1, $2) }
     | value                            { Value $1 }
     | VAR                              { Var $1 }
     | FUNCTION VAR ARROW expr          { Function ($2, $4) }
     | expr expr                        { Application ($1, $2) }
     | IF expr THEN expr ELSE expr      { If ($2, $4, $6) }
     | LPAREN expr COMMA expr RPAREN    { MakePair ($2, $4) }
     | LPAREN expr RPAREN               { $2 }
     | LCURLY record RCURLY                { MakeRec $2 }
     | expr binop expr                  { BinOp ($2, $1, $3) }

value : INT                             { Int $1 }
      | BOOL                            { Bool $1 }
      | CHAR                            { Char $1 }
      | UNIT                            { Unit }

record : VAR COLON expr COMMA record    { ($1, $3) :: $5 }
       | VAR COLON expr                 { ($1, $3) :: [] }


program: expr EOF                       { ([], $1) }