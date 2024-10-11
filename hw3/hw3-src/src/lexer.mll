{
    open Parser
}

(* regular expression *)
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+ 

(* FSA *)
rule read =
parse
| white {read lexbuf}
| "*" {TIMES}
| "+" {PLUS}
| "-" {MINUS}
| "/" {DIV}
| "<=" {LEQ}
| ">=" {GEQ}
| "<" {LE}
| ">" {GE}
| "=" {EQ}
| "&&" {AND}
| "||" {OR}
| "(" {LPAREN}
| ")" {RPAREN}
| "let" {LET}
| "in" {IN}
| "=" {EQ}
| "if" {IF}
| "else" {ELSE}
| "then" {THEN}
| "true" {TRUE}
| "false" {FALSE}
| int {INT (int_of_string (Lexing.lexeme lexbuf))}
| id  {VAR (Lexing.lexeme lexbuf)}
| eof {EOF}



