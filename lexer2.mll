{
open Parser
}

let dig = ['0' - '9']
let int = '-'?dig+

rule read =
  parse 
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| eof { EOF }
