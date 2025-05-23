%{
(* This OCaml code block is included at the beginning of the generated parser file.
   It typically contains `open` statements for modules needed by the semantic actions
   in the grammar rules, and helper function definitions. *)
open Types (* Opens the Types module, providing access to AST node definitions (e.g., ExprNode, Let, If)
             and type definitions used in semantic actions and type annotations. *)
%}

(* Token Declarations:
   These declarations define the terminal symbols (tokens) of the grammar that are
   produced by the lexer.
   - `%token NAME`: Declares a token without an associated semantic value.
   - `%token <type> NAME`: Declares a token that carries a semantic value of the
     specified OCaml `type`. For example, `IDENT` carries a string, `FLOAT` a float. *)
%token <string> IDENT
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
%token GREATER
%token GREATEREQ
%token HASH (* #, used for FinConst k#n *)
%token LT_HASH (* <# *)
%token LEQ_HASH (* <=# *)
%token GT_HASH (* ># *)
%token GEQ_HASH (* >=# *)
%token EQ_HASH (* ==# *)
%token LPAREN RPAREN
%token COMMA
%token EQUAL
%token COLON
%token EOF (* End-of-file token, signaling the end of input. *)
%token OBSERVE
%token FIX
%token COLON_EQUAL (* := for assignment *)
%token NIL          
%token CONS         (* :: for list consing *)
%token MATCH WITH END BAR (* For match expressions *)
%token REF          (* ref for creating references *)
%token BANG         (* ! for dereferencing *)
%token SEMICOLON    (* ; for sequencing *)
(* Distribution specific keywords *)
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

(* Start Symbol:
   `%start <type> name` declares the entry point of the grammar. Parsing begins
   with the non-terminal `name` (here, `prog`), and the parser is expected to
   return a value of the OCaml `type` specified (here, `Types.expr`). *)
%start <Types.expr> prog

(* Type Declarations for Non-terminals:
   `%type <type> name` specifies the OCaml type of the semantic value associated
   with a non-terminal symbol `name`. This type must match the type returned by
   the semantic actions of rules producing this non-terminal. *)
%type <Types.expr> expr assign_expr cons_expr cmp_expr app_expr atomic_expr prefix_expr
%type <Types.expr list> expr_comma_list (* Used for parsing comma-separated lists of expressions for tuples. *)
%type <unit> opt_bar (* Used in match expressions for an optional BAR token. *)
%type <float> number (* Type for the `number` non-terminal, used for parsing distribution parameters. *)
%type <(Types.expr * float) list> distr_cases (* Type for a list of (expression, probability) pairs for DISCRETE. *)

(* Operator Precedence and Associativity:
   These declarations resolve ambiguities in the grammar, such as shift/reduce conflicts,
   by defining the precedence and associativity of operators (tokens).
   - `%left NAME`: Declares `NAME` as a left-associative operator.
   - `%right NAME`: Declares `NAME` as a right-associative operator.
   - `%nonassoc NAME`: Declares `NAME` as non-associative (e.g., `a < b < c` is a syntax error).
   Operators on higher lines have lower precedence. Operators on the same line
   have the same precedence and follow the specified associativity.

   Current Precedence (lowest to highest):
   1. ARROW (left) - Used for `fun x -> e`. Left associativity is standard for function types like `a -> b -> c` meaning `a -> (b -> c)`, but here it's part of the `FUN` rule.
   2. SEMICOLON (left) - For sequencing expressions. Typically very low precedence.
   3. PLUS, MINUS (left) - Arithmetic.
   4. TIMES, DIVIDE (left) - Arithmetic. (Higher than +/- is standard).
   5. OR (left) - Logical OR.
   6. AND (left) - Logical AND.
      Note: Typically, AND has higher precedence than OR. Here, OR is higher than AND.
   7. NOT (right) - Logical NOT. Right associativity is standard for unary prefix operators.
   8. COMMA (right) - Comma operator.
      Note: Right associativity and this high precedence for COMMA is unusual. Commas are
      often left-associative and have very low precedence (for sequence points or tuple construction)
      or are handled as list separators within specific rules (like function calls or list literals)
      without a global precedence. Here it seems to be used for `expr_comma_list`.
   9. Comparison operators (LESS, LESSEQ, etc.) (nonassoc) - Non-associative is standard.
*)
%left ARROW
%left SEMICOLON
%left PLUS MINUS
%left TIMES DIVIDE
%left OR                (* Observation: OR has higher precedence than AND, which is non-standard. Usually AND is higher. *)
%left AND
%right NOT
%right COMMA            (* Observation: COMMA has very high precedence and is right-associative, which is unusual. *)
%nonassoc LESS LESSEQ GREATER GREATEREQ LT_HASH LEQ_HASH GT_HASH GEQ_HASH EQ_HASH

%% 

(* Grammar Rules *)

(* `prog` is the start symbol. It expects an expression followed by EOF (end-of-file).
   The semantic action returns the parsed expression `e`. *)
prog: e = expr EOF { e };

/* `expr` is the main non-terminal for expressions.
   It handles top-level constructs like sequencing (SEMICOLON), LET bindings,
   IF-THEN-ELSE, FUN (lambda abstractions), OBSERVE, FIX (recursion), and MATCH.
   If none of these match, it falls through to `assign_expr` for expressions
   with operators, which have defined precedence.
   The SEMICOLON rule here effectively gives it the lowest precedence among binary operators in `expr`.
*/
expr:
    expr SEMICOLON expr 
    { ExprNode (Seq ($1, $3)) } (* e1 ; e2 *)
  | LET x = IDENT EQUAL e1 = expr IN e2 = expr
    { ExprNode (Let (x, e1, e2)) } (* let x = e1 in e2 *)
  | IF cond = expr THEN e1 = expr ELSE e2 = expr
    { ExprNode (If (cond, e1, e2)) } (* if cond then e1 else e2 *)
  | FUN x = IDENT ARROW e = expr
    { ExprNode (Fun (x, e)) } (* fun x -> e *)
  | OBSERVE e = expr
    { ExprNode (Observe e) } (* observe e *)
  | FIX f = IDENT x = IDENT COLON_EQUAL e = expr (* Note: Using COLON_EQUAL for body definition, unusual for fix. Typically `ARROW` or just `EQUAL`. *)
    { ExprNode (Fix (f, x, e)) } (* fix f x := e *)
  | MATCH e1 = expr WITH opt_bar NIL ARROW e_nil = expr BAR y = IDENT CONS ys = IDENT ARROW e_cons = expr END 
    { ExprNode (MatchList (e1, e_nil, y, ys, e_cons)) } (* match e1 with | nil -> e_nil | y :: ys -> e_cons end *)
  | assign_expr { $1 } (* Fallthrough to expressions with assignment or higher precedence operators *)
  ;

/* `assign_expr` handles assignment expressions.
   Assignment (`:=`) is right-associative due to the recursive call on the RHS.
   If no assignment, falls through to `or_expr`. *)
assign_expr:
  | prefix_expr COLON_EQUAL assign_expr { ExprNode (Assign ($1, $3)) } (* e1 := e2 (associates right-to-left) *)
  | or_expr { $1 }
  ;

/* `or_expr` handles logical OR. Left-associative.
   Falls through to `and_expr`. *)
or_expr:
  | or_expr OR and_expr  { ExprNode (Or ($1, $3)) }
  | and_expr { $1 }
  ;

/* `and_expr` handles logical AND. Left-associative.
   Falls through to `not_expr`. *)
and_expr:
  | and_expr AND not_expr { ExprNode (And ($1, $3)) }
  | not_expr { $1 }
  ;

/* `not_expr` handles logical NOT (prefix operator). Right-associative.
   Falls through to `cmp_expr`. *)
not_expr:
  | NOT not_expr          { ExprNode (Not $2) }
  | cmp_expr { $1 }
  ;

/* `cmp_expr` handles comparison operators.
   Standard comparisons (<, <=, >, >=) are translated:
   - `<` and `<=` are direct (e.g., `Cmp (Lt, $1, $3, false)`).
   - `>` and `>=` are flipped to `<` and `<=` respectively, with a `flipped` flag set to `true`
     (e.g., `e1 > e2` becomes `Cmp (Lt, e2, e1, true)`).
   Finite domain comparisons (<#n, <=#n, etc.) construct `FinCmp` or `FinEq` nodes.
   The integer `n` for modulus is captured from the `INT` token.
   Falls through to `cons_expr`. *)
cmp_expr:
  | cmp_expr LESS cons_expr     { ExprNode (Cmp (Lt, $1, $3, false)) }
  | cmp_expr LESSEQ cons_expr   { ExprNode (Cmp (Le, $1, $3, false)) }
  | cmp_expr GREATER cons_expr  { ExprNode (Cmp (Lt, $3, $1, true)) }
  | cmp_expr GREATEREQ cons_expr { ExprNode (Cmp (Le, $3, $1, true)) }
  | cons_expr LT_HASH INT cons_expr { ExprNode (FinCmp (Lt, $1, $4, $3, false)) }
  | cons_expr LEQ_HASH INT cons_expr { ExprNode (FinCmp (Le, $1, $4, $3, false)) }
  | cons_expr GT_HASH INT cons_expr { ExprNode (FinCmp (Lt, $4, $1, $3, true)) }
  | cons_expr GEQ_HASH INT cons_expr { ExprNode (FinCmp (Le, $4, $1, $3, true)) }
  | cons_expr EQ_HASH INT cons_expr { ExprNode (FinEq ($1, $4, $3)) } (* e.g. e1 ==#n e2 *)
  | cons_expr { $1 }
  ;

/* `cons_expr` handles list construction (`::`). Right-associative.
   e.g., `h :: t` results in `Cons(h, t)`.
   Falls through to `prefix_expr`. *)
cons_expr:
  | prefix_expr CONS cons_expr   { ExprNode (Cons ($1, $3)) } 
  | prefix_expr { $1 }
  ;

/* `prefix_expr` handles prefix operators like `!` (BANG for dereference) and `ref`.
   These are typically right-associative if chained, though not explicitly specified here
   as they are unary. Falls through to `app_expr`. *)
prefix_expr:
  | BANG prefix_expr          { ExprNode (Deref $2) } (* !e *)
  | REF prefix_expr           { ExprNode (Ref $2) }   (* ref e *)
  | app_expr { $1 }
  ;

/* `app_expr` handles function application, `ITERATE`, `FST`, and `SND`.
   Function application (`app_expr atomic_expr`) is left-associative, allowing `f x y` to be `(f x) y`.
   `ITERATE` is a specific form for repeated function application.
   `FST` and `SND` are prefix operators for pair access.
   Falls through to `atomic_expr`. *)
app_expr:
  | app_expr atomic_expr                                                        { ExprNode (FuncApp ($1, $2)) }
  | ITERATE LPAREN e1 = app_expr COMMA e2 = atomic_expr COMMA n = INT RPAREN    { ExprNode (LoopApp (e1, e2, n)) }
  | FST atomic_expr                                                             { ExprNode (First $2) }
  | SND atomic_expr                                                             { ExprNode (Second $2) }
  | atomic_expr                                                                 { $1 }
  ;

/* `atomic_expr` handles the most basic expressions:
   Literals (numbers, TRUE, FALSE, NIL), FinConst (k#n), variables (IDENT),
   parenthesized expressions, tuple construction, and distribution instantiations.
   Unit `()` is also an atomic expression. *)
atomic_expr:
  | n = number                { ExprNode (Const n) } (* Numeric constants parsed by `number` rule *)
  | TRUE                      { ExprNode (BoolConst true) }
  | FALSE                     { ExprNode (BoolConst false) }
  | NIL                       { ExprNode Nil } (* Empty list literal *)
  | k = INT HASH n = INT      (* Finite constant k#n *)
    { if k < 0 || k >= n then failwith (Printf.sprintf "Invalid FinConst: %d#%d. k must be >= 0 and < n." k n) else ExprNode (FinConst (k, n)) }
  | x = IDENT                 { ExprNode (Var x) }
  | DISCRETE LPAREN cases = distr_cases RPAREN (* discrete (p1:e1, p2:e2, ...) *)
    { ExprNode (DistrCase cases) }
  (* Distribution instantiation rules *)
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
  | LPAREN RPAREN { ExprNode Unit } (* Empty parentheses evaluate to Unit *)
  | LPAREN el = expr_comma_list RPAREN
    (* Handles parenthesized expressions and tuple construction:
       - If `expr_comma_list` returns a single expression `[e]`, this rule evaluates to `e` (parenthesized expr).
       - If `expr_comma_list` returns multiple expressions `[e1; e2; ...]`, `build_pairs_from_list`
         constructs a right-nested sequence of `Pair` nodes: `Pair(e1, Pair(e2, ... Pair(en-1, en)...))`.
       The `build_pairs_from_list` helper is defined in the OCaml header block. *)
    { 
      let rec build_pairs_from_list expr_list =
        match expr_list with
        | [] -> failwith "Impossible: empty list in tuple construction - expr_comma_list should be non-empty"
        | [single_e] -> single_e (* Parsed (e), not a pair, return the expression itself. *)
        | first_e :: second_e :: rest_of_list -> (* Parsed (e1, e2, ...), create Pair(e1, rec_parse(e2,...)) *)
            ExprNode (Pair (first_e, build_pairs_from_list (second_e :: rest_of_list)))
      in
      build_pairs_from_list el
    }
  ;

/* `opt_bar` allows an optional BAR `|` at the beginning of match cases,
   e.g., `match e with | C1 -> ...` or `match e with C1 -> ...`. Returns unit. */
opt_bar:
  | /* empty */ { () }
  | BAR         { () }
  ;

/* `distr_cases` parses a comma-separated, non-empty list of `distr_case`
   for discrete distributions. Returns a list of (expression, probability) pairs.
   Uses `separated_nonempty_list` from Menhir's standard library (implicitly). */
distr_cases:
  | /* empty */ { [] } (* Allows empty discrete distribution, though perhaps not meaningful. *)
  | cases = separated_nonempty_list(COMMA, distr_case) { cases }
  ;

/* `distr_case` parses a single branch of a discrete distribution: `p : e`.
   Returns a pair `(e, p)`. */
distr_case:
  | p = number COLON e = expr { (e, p) }
  ;

/* `number` rule parses numeric literals (float or int) and simple arithmetic
   expressions involving numbers. This is used specifically for parsing parameters
   of distributions (e.g., `uniform(0.0, 1.0 + 2.0)`).
   The arithmetic is evaluated at parse time to a float. *)
number:
  | f = FLOAT { f }
  | i = INT   { float_of_int i }
  (* Arithmetic operations on numbers, evaluated during parsing *)
  | e1 = number PLUS e2 = number { e1 +. e2 }
  | e1 = number MINUS e2 = number { e1 -. e2 }
  | e1 = number TIMES e2 = number { e1 *. e2 }
  | e1 = number DIVIDE e2 = number { e1 /. e2 }
  | LPAREN e = number RPAREN { e } (* Parenthesized number expression *)
  ;

/* `expr_comma_list` parses one or more comma-separated expressions.
   Used for constructing tuples (via `atomic_expr`'s LPAREN rule).
   Returns a list of `Types.expr`. *)
expr_comma_list:
    e = expr                         { [e] } (* Single expression in list *)
  | e = expr COMMA rest = expr_comma_list { e :: rest } (* Multiple expressions, prepends e to list from recursive call *)
  ;

%% (* Standard end-of-grammar-rules marker for ocamlyacc/Menhir. *)
