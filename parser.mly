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
%token PLUS MINUS MULT DIV 
 %token <string> ERR_STR
%token <float> FLOAT 
  %token <string> STRING
%token <string> IDENT
%start prog
%type <Ast.expr> prog

%left OR
%left AND
%left NOT
%left EQUAL NE
%left LT LE GT GE
%left PLUS MINUS
%left MULT DIV


%%

prog:
  expr EOF { $1 }
;;

expr:
  INT { Int $1 };;
| expr PLUS expr {Binop(Add, $1, $3)};;
| expr MINUS expr {Binop(Sub, $1, $3)};;
| expr MULT expr {Binop(Mul, $1, $3)};;
