%{
open Ast
open Lexing
%}

%token <int> INT
%token <char> CHAR
%token <string> VAR
%token <string> FILEPATH
%token UNIT WILDCARD
%token TINT TBOOL TCHAR TUNIT
%token LPAREN RPAREN LCURLY RCURLY COMMA COLON EQUALS DOT SEMICOLON
%token TRUE FALSE NOTEQUALS LESS LESSEQ GREATER GREATEREQ NOT AND MUL
%token OR PLUS MINUS
%token LET IN
%token FUNCTION ARROW
%token TYPE
%token IF
%token IMPORT
%token THEN
%token ELSE
%token LEFT RIGHT CASE OF MATCH WITH PIPE BEGIN END
%token FST SND
%token EOF

%type <Ast.pat> pat
%type <Ast.value> value
%type <Ast.binop> binop
%type <Ast.unop> unop
%type <Ast.expr> expr
%type <Ast.vtype> vtype
%type <Ast.def> def
%type <Ast.program> program


%start program

%%

unop : NOT                                  { Not }

binop : EQUALS                              { Eq }
      | NOTEQUALS                           { Neq }
      | LESS                                { Lt }
      | LESSEQ                              { Leq }
      | GREATER                             { Gt }
      | GREATEREQ                           { Geq }
      | AND                                 { And }
      | OR                                  { Or }
      | PLUS                                { Add }
      | MINUS                               { Sub }
      | MUL                                 { Mul }

uexpr : VAR                                 { Var $1 }
      | expr DOT VAR                        { RecAccess ($1, $3) }
      | LPAREN expr RPAREN                  { $2 }
      | value                               { Value $1 }

expr : LET pat EQUALS expr IN expr        { Let ($2, $4, $6) }
     | unop expr                            { UnOp ($1, $2) }
     | IF expr THEN expr ELSE expr          { If ($2, $4, $6) }
     | LPAREN expr COMMA expr RPAREN        { MakePair ($2, $4) }
     | FST expr                             { Fst $2 }
     | SND expr                             { Snd $2 }
     | LCURLY record RCURLY                 { MakeRec $2 }
     | expr binop expr                      { BinOp ($2, $1, $3) }
     | FUNCTION VAR COLON vtype ARROW expr  { MakeFunction ($2, $4, $6) }
     | LEFT LPAREN vtype PLUS vtype RPAREN expr                            { MakeLeft ($3, $5, $7) }
     | RIGHT LPAREN vtype PLUS vtype RPAREN expr                           { MakeRight ($3, $5, $7) }
     | MATCH expr WITH case END             { Match ($2, $4) }
     | CASE expr OF expr PIPE expr          { Case ($2, $4, $6) }
     | IMPORT LPAREN FILEPATH RPAREN        { Import $3 }
     | expr uexpr                           { Application ($1, $2) }
     | uexpr                                { $1 }

value : INT                                 { Int $1 }
      | TRUE                                { Bool true }
      | FALSE                               { Bool false }
      | CHAR                                { Char $1 }
      | UNIT                                { Unit }


bpat : WILDCARD                             { PWild }
     | LPAREN RPAREN                        { PUnit }
     | INT                                  { PInt $1 }
     | TRUE                                 { PBool true }
     | FALSE                                { PBool false }
     | CHAR                                 { PChar $1 }

pat  : bpat                                 { $1 }
     | VAR                                  { PVar $1 }
     | LPAREN pat COMMA pat RPAREN          { PPair ($2, $4)}

case : PIPE pat ARROW expr case             { ($2, $4) :: $5 }
     | PIPE pat ARROW expr                  { ($2, $4) :: [] }

record : VAR COLON expr COMMA record        { ($1, $3) :: $5 }
       | VAR COLON expr                     { ($1, $3) :: [] }

vtype : TINT                                { TInt }
      | TUNIT                               { TUnit }
      | TBOOL                               { TBool }
      | TCHAR                               { TChar }
      | vtype PLUS vtype                    { TSum ($1, $3) }
      | vtype MUL vtype                     { TPair ($1, $3) }
      | vtype ARROW vtype                   { TFunction ($1, $3) }
      | LCURLY trec RCURLY                  { TRecord $2 }
      | VAR                                 { TAlias ($1) }
      
trec : VAR COLON vtype COMMA trec           { RecordType.add $1 $3 $5 }
     | VAR COLON vtype                      { RecordType.add $1 $3 RecordType.empty }

def : LET pat EQUALS expr SEMICOLON         { DVal ($2, $4) }
    | TYPE pat EQUALS vtype SEMICOLON       { DType ($2, $4) }

program : def program                       { let p = $2 in ($1 :: (fst p), snd p)}
        | expr EOF                          { ([], $1) }