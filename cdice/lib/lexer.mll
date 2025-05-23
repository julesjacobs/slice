{
(**
 * @file lexer.mll
 * @brief Lexer for the ContDice language.
 *
 * This file defines the lexical analyzer for ContDice using `ocamllex`.
 * It tokenizes the input string into a stream of tokens defined in `Parser.mly`
 * and handles basic lexical concerns like whitespace, comments, and error reporting.
 *)
open Parser (* To get token definitions *)

(** Custom exception for lexical errors. Contains a descriptive message. *)
exception LexError of string

(**
 * Reports a lexical error with line and column information.
 * @param lexbuf The lexer buffer.
 * @param msg The error message.
 * @raise LexError with a formatted message.
 *)
let error lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise (LexError (Printf.sprintf "Line %d, column %d: %s" line col msg))

(**
 * Updates the lexer buffer's position to the next line.
 * This is called when a newline character is encountered.
 * @param lexbuf The lexer buffer.
 *)
let next_line lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
    pos_lnum = pos.pos_lnum + 1; (* Increment line number *)
    pos_bol = lexbuf.lex_curr_pos; (* Set beginning of line to current position *)
  }

(** Association list of keywords and their corresponding parser tokens. *)
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

(* Lexical definitions (regular expressions for ocamllex) *)
let white = [' ' '\t' '\n' '\r']+ (* Whitespace characters *)
let digit = ['0'-'9'] (* A single digit *)
let int = '-'? digit+ (* Integer: optional minus sign followed by one or more digits *)
let float = '-'? digit+ ('.' digit*)? ('e' ['+' '-']? digit+)? (* Float: standard float format with optional exponent *)
let lower = ['a'-'z'] (* Lowercase letter *)
let upper = ['A'-'Z'] (* Uppercase letter *)
let alpha = lower | upper | '_' (* Alphabetic character or underscore *)
let ident = alpha (alpha | digit)* (* Identifier: starts with alpha, followed by alpha or digit *)
let comment = "(*" [^ '*']* "*)" | "(*" [^ '*']* "*" ([^ ')'] [^ '*']* "*")* ")" (* Nested comments: (* ... *) *)

(**
 * Main tokenizing rule. This is the entry point for the lexer.
 * It tries to match the longest possible prefix of the input stream against the defined patterns.
 *)
rule token = parse
  | white     { token lexbuf } (* Skip whitespace and re-invoke token rule *)
  | '\n'      { next_line lexbuf; token lexbuf } (* Handle newline, update position, and re-invoke *)
  | comment   { token lexbuf } (* Skip comments and re-invoke *)
  | "let"     { LET } (* Keyword 'let' *)
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
  | int as i   { try INT (int_of_string i) (* Integer literal *)
                 with _ -> error lexbuf (Printf.sprintf "Invalid integer literal: %s" i) }
  | float as f { try FLOAT (float_of_string f) (* Float literal *)
                 with _ -> error lexbuf (Printf.sprintf "Invalid float literal: %s" f) }
  | ident as s { try List.assoc s keywords with Not_found -> IDENT s } (* Identifier or keyword *)
  | "<=" "#"   { LEQ_HASH } (* <=# operator for finite set comparison *)
  | "<" "#"    { LT_HASH }  (* <# operator for finite set comparison *)
  | "==" "#"   { EQ_HASH }  (* ==# operator for finite set comparison *)
  | "#"        { HASH }     (* # operator for finite set constant definition k#n *)
  | eof       { EOF }      (* End of file *)
  | _ as c    { error lexbuf (Printf.sprintf "Unexpected character: %c" c) } (* Catch-all for unrecognized characters *)

(**
 * Rule for handling (potentially nested) comments.
 * This rule is called when "(*" is matched by the main `token` rule.
 *)
and comment = parse
  | "*)" { () } (* End of comment *)
  | '\n' { next_line lexbuf; comment lexbuf } (* Newline within a comment, update position *)
  | eof  { error lexbuf "Unterminated comment" } (* Error: end of file reached before comment closes *)
  | _    { comment lexbuf } (* Any other character within the comment *)