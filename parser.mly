%{
open Ast
%}

%token <int> INT
%token IF THEN ELSE FOR WHILE 
%token TRUE FALSE
%token INPUT PRINT  
%token VECTOR MATRIX 
  %token TRANSPOSE DETERMINANT
%token DIM_V DIM_1_M DIM_2_M
  %token ASSIGN
   %token EQUAL LT GT LE GE NE NOT AND OR
    %token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
     %token COMMA SEMICOLON 
  %token EOF
%token PLUS MINUS MULT DIV MOD
 %token <string> ERR_STR
%token <float> FLOAT 
  %token <string> STRING
%token <string> IDENT

%start prog
%type <Ast.expr> prog

                 
%right ASSIGN
%left OR
%left AND
%left NOT
%left EQUAL NE
%left LT LE GT GE
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc IF THEN ELSE


%%

prog:
  expr EOF { $1 }
;;

expr:
  INT { Int $1 };;
| expr PLUS expr {Binop(Add, $1, $3)};;
| expr MINUS expr {Binop(Sub, $1, $3)};;
| expr MULT expr {Binop(Mul, $1, $3)};;
| expr DIV expr {Binop(Div, $1, $3)};;
| expr MOD expr {Binop(Mod, $1, $3)};;
| TRUE { Bool true }
| FALSE { Bool false }
| expr AND expr {Boolop (And, $1, $3)}
| expr OR expr { Boolop (Or, $1, $3) }
| NOT expr {Boolop (Not, $2, Bool false) }
| IF expr THEN expr ELSE expr { IfElse($2, $4, $6) } 
| IDENT { Var $1 } 
| IDENT ;ASSIGN ; expr { Assign($1, $3) }
| LPAREN expr RPAREN { Paren($2) }
| IDENT LPAREN args RPAREN { Func($1, $3)}
| LBRACE expr_list RBRACE { Block($2) }

args: 
    expr { [$1] }
  | expr COMMA args { $1 :: $3 } 
 
expr_list:     
  | expr SEMICOLON expr_list { $1 :: $3 }
  | expr SEMICOLON { [$1] };
  | expr COMMA expr_list { $1 :: $3 }
  | expr { [$1] }
  ;
  
