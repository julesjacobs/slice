{
open Parser

exception LexError of string

let error lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise (LexError (Printf.sprintf "Line %d, column %d: %s" line col msg))

let next_line lexbuf = 
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with 
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = lexbuf.lex_curr_pos;
  }

let keywords = [
  ("let", LET);
  ("in", IN);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);
  ("uniform", UNIFORM);
  ("gaussian", GAUSSIAN);
  ("exponential", EXPONENTIAL);
  ("beta", BETA);
  ("discrete", DISCRETE);
  ("fst", FST);
  ("snd", SND);
  ("fun", FUN);
  ("lognormal", LOGNORMAL);
  ("fix", FIX);
  ("nil", NIL);
  ("match", MATCH);
  ("with", WITH);
  ("end", END);
  ("ref", REF);
]
}

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ ('.' digit*)? ('e' ['+' '-']? digit+)?
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper | '_'
let ident = alpha (alpha | digit)*
let comment = "(*" [^ '*']* "*)" | "(*" [^ '*']* "*" ([^ ')'] [^ '*']* "*")* ")"

rule token = parse
  | white     { token lexbuf }
  | '\n'      { next_line lexbuf; token lexbuf }
  | comment   { token lexbuf }
  | "let"     { LET }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "&&"      { AND }
  | "||"      { OR }
  | "not"     { NOT }
  | "observe" { OBSERVE }
  | "uniform" { UNIFORM }
  | "gaussian" { GAUSSIAN }
  | "normal"  { GAUSSIAN }  (* Alias for gaussian *)
  | "exponential" { EXPONENTIAL }
  | "beta"    { BETA }
  | "lognormal" { LOGNORMAL }
  | "discrete" { DISCRETE }
  | "fst"     { FST }
  | "snd"     { SND }
  | "fun"     { FUN }
  | "->"      { ARROW }
  | '<'       { LESS }
  | "<="      { LESSEQ }
  | '>'       { GREATER }
  | ">="      { GREATEREQ }
  | '='       { EQUAL }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | ','       { COMMA }
  | ':'       { COLON }
  | ":="      { COLON_EQUAL }
  | ";"       { SEMICOLON }
  | "::"      { CONS }
  | "|"       { BAR }
  | '!'       { BANG }
  | "gamma"   { GAMMA }
  | "laplace" { LAPLACE }
  | "cauchy"  { CAUCHY }
  | "weibull" { WEIBULL }
  | "tdist"   { TDIST }
  | "chi2"    { CHI2 }
  | "logistic" { LOGISTIC }
  | "rayleigh" { RAYLEIGH }
  | "pareto" { PARETO }
  | "gumbel1" { GUMBELONE }
  | "gumbel2" { GUMBELTWO }
  | "exppow" { EXPPOW }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { TIMES }
  | '/'       { DIVIDE }
  | "iterate" { ITERATE }
  | int as i   { try INT (int_of_string i)
                 with _ -> error lexbuf (Printf.sprintf "Invalid integer literal: %s" i) }
  | float as f { try FLOAT (float_of_string f)
                 with _ -> error lexbuf (Printf.sprintf "Invalid float literal: %s" f) }
  | ident as s { try List.assoc s keywords with Not_found -> IDENT s }
  | "<=" "#"   { LEQ_HASH }
  | "<" "#"    { LT_HASH }
  | ">=" "#"   { GEQ_HASH }
  | ">" "#"    { GT_HASH }
  | "==" "#"   { EQ_HASH }
  | "#"        { HASH }
  | eof       { EOF }
  | _ as c    { error lexbuf (Printf.sprintf "Unexpected character: %c" c) }

and comment = parse
  | "*)" { () }
  | '\n' { next_line lexbuf; comment lexbuf }
  | eof  { failwith "Unterminated comment" }
  | _    { comment lexbuf }