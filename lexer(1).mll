{
open Parser


type token =
  | INPUT           (* "Input" keyword *)
  | PRINT           (* "Print" keyword *)
  | IF              (* "if" keyword *)
  | THEN            (* "then" keyword *)
  | ELSE            (* "else" keyword *)
  | FOR             (* "for" keyword *)
  | WHILE           (* "while" keyword *)
  | TRUE            (* Boolean constant "true" *)
  | FALSE           (* Boolean constant "false" *)
  | VECTOR
  | TRANSPOSE | DETERMINANT
  | MATRIX
  | DIM_V
  | DIM_1_M
  | DIM_2_M
  | INT of int      (* Integer literal *)
  | FLOAT of float  (* Float literal *)
  | STRING of string (* String literal, e.g., filenames *)
  | IDENT of string (* Identifiers for variable names *)
  | PLUS            (* + operator *)
  | MINUS           (* - operator *)
  | MULT            (* * operator *)
  | DIV             (* / operator *)
  | MOD             (* % operator *)
  | ASSIGN          (* Assignment operator := *)
  | EQUAL           (* Equality operator = *)
  | LT              (* < operator *)
  | GT              (* > operator *)
  | LE              (* <= operator *)
  | GE              (* >= operator *)
  | NE              (* != operator *)
  | NOT             (* Logical not: ! *)
  | AND             (* Logical and: && *)
  | OR              (* Logical or: || *)
  | LPAREN          (* ( *)
  | RPAREN          (* ) *)
  | LBRACKET        (* [ *)
  | RBRACKET        (* ] *)
  | LBRACE          (* { *)
  | RBRACE          (* } *)
  | COMMA           (* , *)
  | SEMICOLON       (* ; *)
  | ERR_STR of string
  | EOF             (* End of file/input *)
}



let digit = ['0'-'9']
let nondigit = ['a'-'z' 'A'-'Z' '_']
let ident = nondigit (nondigit | digit | ['\''])*

let int_literal = digit+
let float_literal = digit+ '.' digit+
let string_literal = "\"" [^ '"']* "\""
let whitespace = [' ' '\t' '\n']+
let line_1_com = "//" [^ '\n']*
let mult_comm = "/*" ([^ '*'] | '*' [^ '/'])* "*/"
let wrong_str = digit+ nondigit+
let vec_dim = "dim_vec "
let mat_dim_1 = "dim_mat_1 " 
let mat_dim_2 = "dim_mat_2 "


rule read = parse
  | [' ' '\t' '\n']+  { read lexbuf }
  | line_1_com {read lexbuf}
  | mult_comm { read lexbuf }
  | wrong_str   as ws    {ERR_STR ws}
  | "Input"         { INPUT }
  | "Print"         { PRINT }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "true"          { TRUE }
  | "false"         { FALSE }


  | ":="            { ASSIGN }

  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { MULT }
  | "/"             { DIV }
  | "%"             { MOD }

  | "<="            { LE }
  | ">="            { GE }
  | "<"             { LT }
  | ">"             { GT }
  | "!="            { NE }
  | "="             { EQUAL }

  | "!"             { NOT }
  | "&&"            { AND }
  | "||"            { OR }

  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | ";"             { SEMICOLON }
  | ","             { COMMA }

  | float_literal as fl  { FLOAT (float_of_string fl) }
  | int_literal as int_str { INT (int_of_string int_str) }
  | string_literal as s   { 
                              let len = String.length s in
                              STRING (String.sub s 1 (len - 2))
                           }
  | mat_dim_1       {DIM_1_M}
  | mat_dim_2       {DIM_2_M}
  | vec_dim         {DIM_V}
  | "trans_mat"     { TRANSPOSE }
  | "Vector"        {VECTOR }
  | "Matrix"        {MATRIX}
  | "det_mat"       {DETERMINANT}
  | ident as id { IDENT id }

  | eof             { EOF }
  | _               { failwith ("Unrecognized token: " ^ Lexing.lexeme lexbuf) }




