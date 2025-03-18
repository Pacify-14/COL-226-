{
(* Header Section: OCaml code that is copied verbatim *)
open Printf

(* Token type definition *)
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

(* Regular Expression Macros *)
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


rule token = parse
  | whitespace { token lexbuf }
  | line_1_com {token lexbuf}
  | mult_comm { token lexbuf }
  | wrong_str   as ws    {ERR_STR ws}
  (* Keywords *)
  | "Input"         { INPUT }
  | "Print"         { PRINT }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "true"          { TRUE }
  | "false"         { FALSE }


  (* Assignment operator *)
  | ":="            { ASSIGN }

  (* Arithmetic Operators *)
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { MULT }
  | "/"             { DIV }
  | "%"             { MOD }

  (* Comparison Operators *)
  | "<="            { LE }
  | ">="            { GE }
  | "<"             { LT }
  | ">"             { GT }
  | "!="            { NE }
  | "="             { EQUAL }

  (* Boolean Operators *)
  | "!"             { NOT }
  | "&&"            { AND }
  | "||"            { OR }

  (* Delimiters *)
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | ";"             { SEMICOLON }
  | ","             { COMMA }

  (* Literals *)
  | float_literal as fl  { FLOAT (float_of_string fl) }
  | int_literal as int_str { INT (int_of_string int_str) }
  | string_literal as s   { 
                              let len = String.length s in
                              STRING (String.sub s 1 (len - 2))
                           }
  | mat_dim_1       {DIM_1_M}
  | mat_dim_2       {DIM_2_M}
  | vec_dim         {DIM_V}
  (* Identifiers *)
  | "trans_mat"     { TRANSPOSE }
  | "Vector"        {VECTOR }
  | "Matrix"        {MATRIX}
  | "det_mat"       {DETERMINANT}
  | ident as id { IDENT id }

  | eof             { EOF }
  | _               { failwith ("Unrecognized token: " ^ Lexing.lexeme lexbuf) }

{
(* Trailing OCaml Code: Testing the Lexer *)

(* Helper function to convert tokens to a string for printing *)
let string_of_token token =
  match token with
  | ERR_STR s    -> "ERR_STR(" ^ s ^ ")"
  | DIM_1_M      -> "DIM_1_M"
  | DIM_2_M      -> "DIM_2_M"
  | DIM_V           -> "DIM_V"
  | INPUT         -> "INPUT"
  | VECTOR        -> "VECTOR"
  | MATRIX        -> "MATRIX"
  | TRANSPOSE     -> "TRANSPOSE"
  | DETERMINANT   -> "DETERMINANT"
  | PRINT         -> "PRINT"
  | IF            -> "IF"
  | THEN          -> "THEN"
  | ELSE          -> "ELSE"
  | FOR           -> "FOR"
  | WHILE         -> "WHILE"
  | TRUE          -> "TRUE"
  | FALSE         -> "FALSE"
  | INT n         -> "INT(" ^ string_of_int n ^ ")"
  | FLOAT f       -> "FLOAT(" ^ string_of_float f ^ ")"
  | STRING s      -> "STRING(" ^ s ^ ")"
  | IDENT s       -> "IDENT(" ^ s ^ ")"
  | PLUS          -> "PLUS"
  | MINUS         -> "MINUS"
  | MULT          -> "MULT"
  | DIV           -> "DIV"
  | MOD           -> "MOD"
  | ASSIGN        -> "ASSIGN"
  | EQUAL         -> "EQUAL"
  | LT            -> "LT"
  | GT            -> "GT"
  | LE            -> "LE"
  | GE            -> "GE"
  | NE            -> "NE"
  | NOT           -> "NOT"
  | AND           -> "AND"
  | OR            -> "OR"
  | LPAREN        -> "LPAREN"
  | RPAREN        -> "RPAREN"
  | LBRACKET      -> "LBRACKET"
  | RBRACKET      -> "RBRACKET"
  | LBRACE        -> "LBRACE"
  | RBRACE        -> "RBRACE"
  | COMMA         -> "COMMA"
  | SEMICOLON     -> "SEMICOLON"
  | EOF           -> "EOF"

(* Recursively print tokens until EOF is reached *)
let rec print_tokens lexbuf =
  let tok = token lexbuf in
  Printf.printf "%s\n" (string_of_token tok);
  if tok <> EOF then print_tokens lexbuf

(* Main entry point for testing the lexer *)
let () =
  let lexbuf = Lexing.from_channel stdin in
  print_tokens lexbuf
}


