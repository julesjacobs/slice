{
(* This section contains OCaml code that is included in the generated lexer.
   It typically includes opening modules, defining helper functions,
   and declaring exceptions. *)
open Parser (* Opens the Parser module to access token definitions (e.g., LET, IF, IDENT). *)

(** Exception [LexError] is raised when the lexer encounters a sequence of
    characters that does not match any of the defined lexical rules.
    The string argument carries an error message, typically including line
    and column information provided by the `error` helper function. *)
exception LexError of string

(** [error lexbuf msg] is a helper function to raise a [LexError].
    It captures the current position (line number and column) from the
    lexing buffer [lexbuf] and formats it into the error message [msg]. *)
let error lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise (LexError (Printf.sprintf "Line %d, column %d: %s" line col msg))

(** [next_line lexbuf] updates the lexer's current position to reflect
    that a newline character has been processed. It increments the line
    number and resets the beginning-of-line position for column calculation. *)
let next_line lexbuf = 
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with 
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = lexbuf.lex_curr_pos;
  }

(** [keywords] is an association list mapping keyword strings (e.g., "let", "if")
    to their corresponding token types defined in the parser (e.g., `Parser.LET`,
    `Parser.IF`). This table is used by the `ident` rule to distinguish
    keywords from general identifiers. *)
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

(* Lexer regular expression definitions *)
let white = [' ' '\t' '\n' '\r']+ (* Matches one or more whitespace characters (excluding newline if handled separately). *)
let digit = ['0'-'9'] (* Matches any single digit. *)
let int = '-'? digit+ (* Matches an optional minus sign followed by one or more digits. *)
let float = '-'? digit+ ('.' digit*)? ('e' ['+' '-']? digit+)? (* Matches a float: optional sign, digits, optional decimal part, optional exponent. *)
let lower = ['a'-'z'] (* Matches any lowercase letter. *)
let upper = ['A'-'Z'] (* Matches any uppercase letter. *)
let alpha = lower | upper | '_' (* Matches any letter or underscore. *)
let ident = alpha (alpha | digit)* (* Matches an identifier: starts with alpha, followed by zero or more alphanumeric or underscore. *)
(* `comment` regexp: attempts to match block comments like (* ... *) or (* ... (* ... *) ... *).
   This specific regular expression aims to handle some forms of nested comments, but
   complex or deeply nested structures might be better handled by a dedicated recursive rule.
   In the `rule token` below, this regexp variable is used to match and skip such comments. *)
let comment = "(*" [^ '*']* "*)" | "(*" [^ '*']* "*" ([^ ')'] [^ '*']* "*")* ")"

(** [rule token lexbuf] is the main entry point for the lexer.
    It attempts to match the longest possible sequence of characters at the
    current position in the lexing buffer [lexbuf] against the defined rules.
    When a rule matches, the corresponding OCaml action is executed,
    which typically returns a token to the parser. *)
rule token = parse
  | white     { token lexbuf } (* Skip whitespace by recursively calling token. *)
  | '\n'      { next_line lexbuf; token lexbuf } (* Update line number and then skip. *)
  | comment   { token lexbuf } (* Matches comments using the `comment` regexp variable defined above and skips them. *)
  (* Keywords: Matched directly and return their specific token types. *)
  | "let"     { LET }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "true"    { TRUE }
  | "false"   { FALSE }
  (* Operators and Punctuation: Matched directly. *)
  | "&&"      { AND }
  | "||"      { OR }
  | "not"     { NOT }
  | "observe" { OBSERVE }
  | "uniform" { UNIFORM }
  | "gaussian" { GAUSSIAN }
  | "normal"  { GAUSSIAN }  (* "normal" is an alias for "gaussian" token. *)
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
  (* Distribution keywords *)
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
  (* Arithmetic operators *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { TIMES }
  | '/'       { DIVIDE }
  | "iterate" { ITERATE }
  (* Integer literals: Matched by the `int` regexp.
     The matched string `i` is converted to an OCaml `int` using `int_of_string`.
     Returns `INT token` with the value. Raises `LexError` if conversion fails (e.g., overflow). *)
  | int as i   { try INT (int_of_string i)
                 with _ -> error lexbuf (Printf.sprintf "Invalid integer literal: %s" i) }
  (* Float literals: Matched by the `float` regexp.
     The matched string `f` is converted to an OCaml `float` using `float_of_string`.
     Returns `FLOAT token` with the value. Raises `LexError` if conversion fails. *)
  | float as f { try FLOAT (float_of_string f)
                 with _ -> error lexbuf (Printf.sprintf "Invalid float literal: %s" f) }
  (* Identifiers: Matched by the `ident` regexp.
     The matched string `s` is first looked up in the `keywords` table.
     If found, the corresponding keyword token is returned.
     Otherwise, it's treated as a general identifier and `IDENT s` is returned. *)
  | ident as s { try List.assoc s keywords with Not_found -> IDENT s }
  (* Composite tokens for finite domain comparisons. *)
  | "<=" "#"   { LEQ_HASH } (* e.g., <=# *)
  | "<" "#"    { LT_HASH }  (* e.g., <#  *)
  | ">=" "#"   { GEQ_HASH } (* e.g., >=# *)
  | ">" "#"    { GT_HASH }  (* e.g., >#  *)
  | "==" "#"   { EQ_HASH }  (* e.g., ==# *)
  | "#"        { HASH }     (* Standalone hash, potentially for modulus in FinConst. *)
  | eof       { EOF }      (* End-of-file token. *)
  (* Error catch-all: If none of the above rules match, it's an unexpected character.
     The character `c` is captured and an error is raised using the `error` helper. *)
  | _ as c    { error lexbuf (Printf.sprintf "Unexpected character: %c" c) }

(** [and comment lexbuf] defines a separate recursive lexing rule (function)
    specifically for handling the content *inside* multi-line comments that
    start with `(*` and end with `*)`.
    This function would typically be invoked from the main `token` rule when
    a `(*` sequence is recognized.
    Note: The current `rule token` uses a complex `comment` regexp variable to handle
    comments directly, and does not explicitly call this `comment` function.
    This function is therefore likely unused in the current lexer setup as-is.
    If it were used, its behavior would be:
    - If `*)` is found, the comment is terminated, and it returns `()`.
    - If a newline `\n` is found, `next_line` is called to update position,
      and then it recursively calls itself to continue scanning the comment.
    - If `eof` (end-of-file) is reached before `*)`, it's an unterminated comment,
      and it raises a `Failure` exception.
    - Any other character is consumed, and it recursively calls itself.
*)
and comment = parse
  | "*)" { () } (* End of comment marker. *)
  | '\n' { next_line lexbuf; comment lexbuf } (* Newline within comment. *)
  | eof  { failwith "Unterminated comment" } (* EOF found before comment termination. *)
  | _    { comment lexbuf } (* Consume any other character and continue. *)