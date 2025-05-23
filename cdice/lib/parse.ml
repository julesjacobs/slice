(**
 * @file parse.ml
 * @brief Provides the main parsing function for ContDice expressions.
 *
 * This module exposes a single function, `parse_expr`, which takes a string
 * representation of a ContDice expression and returns its Abstract Syntax Tree (AST)
 * representation as defined in `Ast.ml`. It utilizes the lexer (`Lexer.mll`)
 * and parser (`Parser.mly`) to perform this conversion and handles common
 * parsing and lexing errors by raising `Failure` with descriptive messages.
 *)

open Types (* To access Ast module alias *)
open Ast (* To get Ast.expr into scope *)
open Lexing (* For lexer buffer and position management *)

(**
 * @function parse_expr
 * @brief Parses a string into a ContDice expression (AST).
 *
 * Creates a lexer buffer from the input string, then feeds it to the
 * Menhir-generated parser (`Parser.prog`) using the ocamllex-generated
 * lexer (`Lexer.token`).
 *
 * @param s The string containing the ContDice expression to parse.
 * @return The `Ast.expr` representing the parsed expression.
 * @raise Failure if a lexical error (from `Lexer.LexError`) or a syntax error
 *        (from `Parser.Error`) occurs. The failure message includes details
 *        about the error, including line and column numbers for syntax errors.
 *)
let parse_expr (s : string) : expr =
  let lexbuf = Lexing.from_string s in (* Create a lexer buffer from the input string *)
  try
    (* Run the parser. Parser.prog is the entry point defined in parser.mly,
       Lexer.token is the token-producing function from lexer.mll. *)
    Parser.prog Lexer.token lexbuf
  with
  (* Handle lexical errors *)
  | Lexer.LexError msg ->
      failwith (Printf.sprintf "Lexical error: %s" msg)
  (* Handle syntax errors *)
  | Parser.Error ->
      (* Menhir raises Parser.Error on syntax errors.
         We extract position information from the lexer buffer to provide a helpful message. *)
      let pos = Lexing.lexeme_start_p lexbuf in (* Position where the error occurred *)
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in (* Column relative to the beginning of the line *)
      let token = Lexing.lexeme lexbuf in (* The token that caused the syntax error *)
      failwith
        (Printf.sprintf
           "Parse error at Line %d, Column %d: Unexpected token '%s'" line col
           token)
