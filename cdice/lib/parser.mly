%{
(**
 * @file parser.mly
 * @brief Parser for the ContDice language.
 *
 * This file defines the grammar for ContDice using `menhir`.
 * It specifies how tokens from `lexer.mll` are combined to form
 * Abstract Syntax Tree (AST) nodes defined in `ast.ml`.
 * The parser handles operator precedence and associativity.
 *)
open Types (* Provides access to Ast, TypeSystem etc. via module aliases *)
open Ast   (* Brings Ast.expr, Ast.ExprNode, and other AST types directly into scope *)
%}

(** Tokens defined by the lexer, with their semantic values if any. *)
%token <string> IDENT (* Identifier, e.g., variable names *)
%token <float> FLOAT
%token <int> INT
%token LET IN
%token IF THEN ELSE
%token TRUE FALSE
%token AND OR NOT
%token UNIFORM
%token GAUSSIAN
%token EXPONENTIAL
%token BETA
%token LOGNORMAL
%token DISCRETE
%token FST SND
%token FUN ARROW
%token LESS
%token LESSEQ
%token HASH
%token LT_HASH
%token LEQ_HASH
%token EQ_HASH
%token LPAREN RPAREN
%token COMMA
%token EQUAL
%token COLON
%token EOF
%token OBSERVE
%token FIX
%token COLON_EQUAL (* := *)
%token NIL          
%token CONS         (* :: *)
%token MATCH WITH END BAR
%token REF          (* ref *)
%token BANG         (* ! *)
%token SEMICOLON    (* ; *)
%token GAMMA
%token LAPLACE
%token CAUCHY
%token WEIBULL
%token TDIST
%token CHI2
%token LOGISTIC
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token RAYLEIGH
%token PARETO
%token GUMBELONE
%token GUMBELTWO
%token EXPPOW
%token ITERATE
(* %token FOR TO DO *)

%start <expr> prog

(* Define types for non-terminals *) 
%type <expr> expr assign_expr cons_expr cmp_expr app_expr atomic_expr prefix_expr (* Type for expression-like non-terminals *)
%type <expr list> expr_comma_list (* Type for comma-separated lists of expressions, used in tuples *)
%type <unit> opt_bar (* Type for optional bar in match cases *)
%type <float> number (* Type for numeric literals and simple arithmetic results within 'atomic_expr' *)
%type <(expr * float) list> distr_cases (* Type for cases in a discrete distribution: list of (expression, probability) *)

(** Operator precedence and associativity.
    Lower lines in this list have higher precedence. *)
%left ARROW             (* Function type arrow `->`, low precedence, left-associative (though often used like right for currying) *)
%left SEMICOLON         (* Sequencing `;`, left-associative *)
(* Arithmetic operators for 'number' non-terminal - not for general expressions *)
%left PLUS MINUS        (* Addition and subtraction, left-associative *)
%left TIMES DIVIDE      (* Multiplication and division, left-associative *)
%left OR                (* Logical OR `||`, left-associative *)
%left AND               (* Logical AND `&&`, left-associative *)
%right NOT              (* Logical NOT `not`, right-associative (unary prefix) *)
%right COMMA            (* Comma operator for tuples, right-associative to build pairs like (e1, (e2, e3)) *)
%nonassoc LESS LESSEQ LT_HASH LEQ_HASH EQ_HASH (* Comparison operators, non-associative *)
%nonassoc IF THEN ELSE (* If-then-else, non-associative to handle dangling else (though grammar structure also helps) *)
%nonassoc LET IN        (* Let-in, non-associative *)
%nonassoc FUN           (* Fun, non-associative *)
%nonassoc OBSERVE       (* Observe, non-associative *)
%nonassoc FIX           (* Fix, non-associative *)
%nonassoc MATCH         (* Match, non-associative *)
%right CONS             (* List cons `::`, right-associative *)
%right COLON_EQUAL     (* Assignment `:=`, right-associative *)
%nonassoc BANG          (* Dereference `!`, non-associative (prefix unary) *)
%nonassoc REF           (* Reference `ref`, non-associative (prefix unary) *)


%%

(** The main entry point for parsing a ContDice program.
    A program is an expression followed by the end-of-file token. *)
prog: e = expr EOF { e };

(** General expressions, ordered by precedence from lowest to highest. *)

(** Expression level: LET, IF, FUN, OBSERVE, FIX, MATCH.
    These are typically block-structured or have specific keywords.
    Sequencing (e1 ; e2) is also handled at a low precedence level. *)
expr:
    expr SEMICOLON expr
    { ExprNode (Seq ($1, $3)) } (* e1 ; e2 *)
  | LET x = IDENT EQUAL e1 = expr IN e2 = expr
    { ExprNode (Let (x, e1, e2)) } (* let x = e1 in e2 *)
  | IF cond = expr THEN e1 = expr ELSE e2 = expr
    { ExprNode (If (cond, e1, e2)) }
  | FUN x = IDENT ARROW e = expr
    { ExprNode (Fun (x, e)) }
  | OBSERVE e = expr
    { ExprNode (Observe e) }
  | FIX f = IDENT x = IDENT COLON_EQUAL e = expr 
    { ExprNode (Fix (f, x, e)) }
  | MATCH e1 = expr WITH opt_bar NIL ARROW e_nil = expr BAR y = IDENT CONS ys = IDENT ARROW e_cons = expr END 
    { ExprNode (MatchList (e1, e_nil, y, ys, e_cons)) } 
  (*
  | FOR i = IDENT EQUAL n1 = expr TO n2 = expr DO body = expr
    { ExprNode (For (i, n1, n2, body)) }
  *)
  | assign_expr { $1 } /* Fallthrough to assign_expr */
  ;

/* Assignment level (right-associative due to COLON_EQUAL being %right) */
assign_expr:
  | prefix_expr COLON_EQUAL assign_expr { ExprNode (Assign ($1, $3)) } (* ref_expr := val_expr *)
  | or_expr { $1 } (* Fallthrough to higher precedence *)
  ;

/* OR Level (left-associative) */
or_expr:
  | or_expr OR and_expr  { ExprNode (Or ($1, $3)) } (* e1 || e2 *)
  | and_expr { $1 }     (* Fallthrough to higher precedence *)
  ;

/* AND Level (left-associative) */
and_expr:
  | and_expr AND not_expr { ExprNode (And ($1, $3)) } (* e1 && e2 *)
  | not_expr { $1 }      (* Fallthrough to higher precedence *)
  ;

/* NOT Level (right-associative prefix operator) */
not_expr:
  | NOT not_expr          { ExprNode (Not $2) } (* not e *)
  | cmp_expr { $1 }       (* Fallthrough to higher precedence *)
  ;

/* Comparison level (non-associative) */
cmp_expr:
  | cmp_expr LESS cons_expr     { ExprNode (Less ($1, $3)) }     (* e1 < e2 *)
  | cmp_expr LESSEQ cons_expr   { ExprNode (LessEq ($1, $3)) }   (* e1 <= e2 *)
  | cons_expr LT_HASH INT cons_expr { ExprNode (FinLt ($1, $4, $3)) }  (* e1 <#k e2 *)
  | cons_expr LEQ_HASH INT cons_expr { ExprNode (FinLeq ($1, $4, $3)) } (* e1 <=#k e2 *)
  | cons_expr EQ_HASH INT cons_expr { ExprNode (FinEq ($1, $4, $3)) } (* e1 ==#k e2 *)
  | cons_expr { $1 }            (* Fallthrough to higher precedence *)
  ;

/* Cons level for list construction (right-associative due to CONS being %right) */
cons_expr:
  | prefix_expr CONS cons_expr   { ExprNode (Cons ($1, $3)) } (* e_head :: e_tail *)
  | prefix_expr { $1 }           (* Fallthrough to higher precedence *)
  ;

/* Prefix operators level for `!` (deref) and `ref` (non-associative prefix) */
prefix_expr:
  | BANG prefix_expr          { ExprNode (Deref $2) } (* !e *)
  | REF prefix_expr           { ExprNode (Ref $2) }   (* ref e *)
  | app_expr { $1 }           (* Fallthrough to higher precedence *)
  ;

/* Application Level (left-associative for function application) */
app_expr:
  | app_expr atomic_expr                                                        { ExprNode (FuncApp ($1, $2)) } (* f x *)
  | ITERATE LPAREN e1 = app_expr COMMA e2 = atomic_expr COMMA n = INT RPAREN    { ExprNode (LoopApp (e1, e2, n)) } (* iterate(f, x, n) *)
  | FST atomic_expr                                                             { ExprNode (First $2) }  (* fst e *)
  | SND atomic_expr                                                             { ExprNode (Second $2) }  (* snd e *)
  | atomic_expr                                                                 { $1 }        (* Fallthrough to highest precedence *)
  ;

/* Atomic expressions: highest precedence, basic constructs. */
atomic_expr:
  | n = number                { ExprNode (Const n) } (* Numeric literal (can be simple arithmetic from 'number' rule) *)
  | TRUE                      { ExprNode (BoolConst true) } (* true *)
  | FALSE                     { ExprNode (BoolConst false) }
  | NIL                       { ExprNode Nil }
  | k = INT HASH n = INT
    { if k < 0 || k >= n then raise Parser.Error else ExprNode (FinConst (k, n)) }
  | x = IDENT                 { ExprNode (Var x) }
  | DISCRETE LPAREN cases = distr_cases RPAREN
    { ExprNode (DistrCase cases) }
  | UNIFORM LPAREN lo = app_expr COMMA hi = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DUniform, lo, hi))) }
  | GAUSSIAN LPAREN mean = app_expr COMMA std = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DGaussian, mean, std))) }
  | EXPONENTIAL LPAREN rate = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DExponential, rate))) }
  | BETA LPAREN alpha = app_expr COMMA beta = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DBeta, alpha, beta))) }
  | LOGNORMAL LPAREN mu = app_expr COMMA sigma = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DLogNormal, mu, sigma))) }
  | GAMMA LPAREN shape = app_expr COMMA scale = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DGamma, shape, scale))) }
  | LAPLACE LPAREN scale = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DLaplace, scale))) }
  | CAUCHY LPAREN scale = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DCauchy, scale))) }
  | WEIBULL LPAREN a = app_expr COMMA b = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DWeibull, a, b))) }
  | TDIST LPAREN nu = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DTDist, nu))) }
  | CHI2 LPAREN nu = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DChi2, nu))) }
  | LOGISTIC LPAREN scale = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DLogistic, scale))) }
  | RAYLEIGH LPAREN sigma = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DRayleigh, sigma))) }
  | PARETO LPAREN xm = app_expr COMMA alpha = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DPareto, xm, alpha))) }
  | GUMBELONE LPAREN mu = app_expr COMMA beta_param = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DGumbel1, mu, beta_param))) }
  | GUMBELTWO LPAREN mu = app_expr COMMA beta_param = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DGumbel2, mu, beta_param))) }
  | EXPPOW LPAREN arg1 = app_expr COMMA arg2 = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DExppow, arg1, arg2))) }
  | LPAREN RPAREN { ExprNode Unit } 
  | LPAREN el = expr_comma_list RPAREN (* New rule for (e1), (e1,e2), (e1,e2,e3), etc. *)
    { 
      let rec build_pairs_from_list expr_list =
        match expr_list with
        | [] -> failwith "Impossible: empty list in tuple construction - expr_comma_list should be non-empty"
        | [single_e] -> single_e (* Parsed (e), not a pair *)
        | first_e :: second_e :: rest_of_list -> (* Parsed (e1, e2, ...), create Pair(e1, rec_parse(e2,...)) *)
            ExprNode (Pair (first_e, build_pairs_from_list (second_e :: rest_of_list)))
      in
      build_pairs_from_list el
    }
  ;

/* Optional bar `|` before the first case in a match statement. */
opt_bar:
  | /* empty */ { () }
  | BAR         { () }
  ;

/* Rule for parsing the (expression : probability) pairs for discrete distributions.
   Uses Menhir's standard library for `separated_nonempty_list`. */
distr_cases:
  | /* empty */ { [] } (* Allows `discrete()` though perhaps not semantically valid if empty. *)
  | cases = separated_nonempty_list(COMMA, distr_case) { cases }
  ;

distr_case:
  | p = number COLON e = expr { (e, p) } (* prob : expr *)
  ;

/* 'number' rule: Allows simple arithmetic within certain atomic expressions,
   specifically for probabilities in DistrCase and parameters of distributions if they were constants.
   Note: This 'number' arithmetic is distinct from general expression arithmetic.
   The precedence for PLUS, MINUS, TIMES, DIVIDE here is local to the 'number' non-terminal. */
number:
  | f = FLOAT { f } (* Float literal *)
  | i = INT   { float_of_int i } (* Integer literal, converted to float *)
  /* Simple arithmetic for constant folding at parse time for numbers. */
  | e1 = number PLUS e2 = number { e1 +. e2 }
  | e1 = number MINUS e2 = number { e1 -. e2 }
  | e1 = number TIMES e2 = number { e1 *. e2 }
  | e1 = number DIVIDE e2 = number { e1 /. e2 }
  | LPAREN e = number RPAREN { e } (* Parenthesized number *)
  ;

/* Definition for comma-separated list of expressions, used for tuples.
   e.g., (e1), (e1, e2), (e1, e2, e3).
   A single expression in parentheses is handled by `atomic_expr: LPAREN expr RPAREN`
   which gets disambiguated correctly by Menhir if `expr_comma_list` doesn't match just one `expr`.
   The current `atomic_expr` rule for `LPAREN el = expr_comma_list RPAREN`
   handles both single expressions in parens and actual tuples.
*/
expr_comma_list:
    e = expr                         { [e] } (* A single expression can be a list of one. *)
  | e = expr COMMA rest = expr_comma_list { e :: rest } (* Comma-separated, builds list `e1 :: e2 :: ... :: en :: []`. *)
  ;

%%
