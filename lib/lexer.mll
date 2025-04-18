{
open Parser

exception LexError of string

let error lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise (LexError (Printf.sprintf "Line %d, column %d: %s" line col msg))
}

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ ('.' digit*)? ('e' ['+' '-']? digit+)?
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper | '_'
let ident = alpha (alpha | digit)*

rule token = parse
  | white     { token lexbuf }
  | "let"     { LET }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "uniform" { UNIFORM }
  | '<'       { LESS }
  | '='       { EQUAL }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | ','       { COMMA }
  | float as f { try FLOAT (float_of_string f)
                 with _ -> error lexbuf (Printf.sprintf "Invalid float literal: %s" f) }
  | ident as s { IDENT s }
  | eof       { EOF }
  | _ as c    { error lexbuf (Printf.sprintf "Unexpected character: %c" c) }