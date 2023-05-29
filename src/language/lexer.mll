{
  open Batteries;;

  open Lexing;;
  open Parser;;
  open Printf;;

  exception LexerError of string;;
}

let digit = ['0'-'9']
let integer = digit+

let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_cont = ident_start | ['0'-'9']
let ident = ident_start ident_cont*

let whitespace = [' ' '\t']+

rule token = parse
  | whitespace { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | integer as x { INTEGER(x) }
  | "after" { AFTER }
  | "before" { BEFORE }
  | "print" { PRINT }
  | "isint" { ISINT }
  | "isbool" { ISBOOL }
  | "istuple" { ISTUPLE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "," { COMMA }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "<" { LESS }
  | ">" { GREATER }
  | "&&" { BOOL_AND }
  | "||" { BOOL_OR }
  | "let" { LET }
  | "=" { EQUAL }
  | "in" { IN }
  | ":=" { COLON_EQUAL }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "def" { DEF }
  | "end" { END }
  | ident as x { IDENTIFIER x }
  | eof { EOF }
  | _ as c { raise (LexerError (sprintf "unrecognized character: %c" c)) }
