{
open Parser
}

let digit = ['0' - '9']
let int = digit+
          let nondigit = ['a'-'z' 'A'-'Z' '_']
let ident = nondigit (nondigit | digit | ['\'']) *
            let int_literal = digit+          

rule read =
  parse
    [' ' '\t' '\n'] {read lexbuf}
        | "+" {PLUS}               
        | "-" {MINUS}
        | '*' {MULT}
        | int_literal as int_str { INT (int_of_string int_str) }

        | eof { EOF }
