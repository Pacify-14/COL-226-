{
open Parser
}

let digit = ['0' - '9']
          let nondigit = ['a'-'z' 'A'-'Z' '_']
let ident = nondigit (nondigit | digit | ['\'']) *
            let int_literal = digit+          
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

rule read =
  parse
    [' ' '\t' '\n']+ {read lexbuf}
        | line_1_com {read lexbuf}
        | mult_comm {read lexbuf}                
        | "+" {PLUS}               
        | "-" {MINUS}
        | '*' {MULT}
        | "/" {DIV}       
        | "%" {MOD}
                | int_literal as int_str { INT (int_of_string int_str) }
        | "Input" {INPUT}
        | "Print" {PRINT}
        | "if" {IF}
        | "then" {THEN}
        | "else" {ELSE}
        | "for"  {FOR}
        | "while" {WHILE}
        | "true" {TRUE} 
        | "false" {FALSE}
        | ":=" {ASSIGN}
        | "!" {NOT} 
        | "&&" {AND}
        | "||" {OR}          
        | eof { EOF }
