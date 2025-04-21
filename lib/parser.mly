%{
open Types
%}

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
%token DISCRETE
%token FST SND
%token FUN ARROW
%token LESS
%token LESSEQ
%token HASH
%token LT_HASH
%token LEQ_HASH
%token LPAREN RPAREN
%token COMMA
%token EQUAL
%token COLON
%token EOF

%start <Types.expr> prog

(* Define types for non-terminals *) 
%type <Types.expr> expr or_expr and_expr not_expr cmp_expr app_expr atomic_expr
%type <float> number
%type <(Types.expr * float) list> distr_cases

(* Operator precedence and associativity *)
%left ARROW             (* Function type arrow *)
%left OR                (* OR has lower precedence than AND *)
%left AND               (* AND has lower precedence than NOT/comparison *)
%right NOT              (* Unary NOT has high precedence *)
%nonassoc LESS LESSEQ LT_HASH LEQ_HASH (* Comparison operators *)
(* Application (juxtaposition) has higher precedence - handled by grammar structure *)
(* FST, SND have high precedence - handled by grammar structure *)

%% 

prog: e = expr EOF { e };

/* Lowest precedence: LET, IF, FUN (handled implicitly by grammar structure now) */
expr:
  | LET x = IDENT EQUAL e1 = expr IN e2 = expr
    { ExprNode (Let (x, e1, e2)) }
  | IF cond = expr THEN e1 = expr ELSE e2 = expr
    { ExprNode (If (cond, e1, e2)) }
  (* Removed the unsupported IF without ELSE rule to avoid ambiguity for now *)
  (* | IF cond = expr THEN e1 = expr
    { let _ = cond in let _ = e1 in failwith "If without else is not supported" } *)
  | FUN x = IDENT ARROW e = expr
    { ExprNode (Fun (x, e)) }
  | or_expr { $1 } /* Fallthrough */
  ;

/* OR Level */
or_expr:
  | or_expr OR and_expr  { ExprNode (Or ($1, $3)) }
  | and_expr { $1 }     /* Fallthrough */
  ;

/* AND Level */
and_expr:
  | and_expr AND not_expr { ExprNode (And ($1, $3)) }
  | not_expr { $1 }      /* Fallthrough */
  ;

/* NOT Level */
not_expr:
  | NOT not_expr          { ExprNode (Not $2) }
  | cmp_expr { $1 }       /* Fallthrough */
  ;

/* Comparison level */
cmp_expr:
  | cmp_expr LESS app_expr     { ExprNode (Less ($1, $3)) }
  | cmp_expr LESSEQ app_expr   { ExprNode (LessEq ($1, $3)) }
  | app_expr LT_HASH INT app_expr { ExprNode (FinLt ($1, $4, $3)) }
  | app_expr LEQ_HASH INT app_expr { ExprNode (FinLeq ($1, $4, $3)) }
  | app_expr { $1 }            /* Fallthrough */
  ;

/* Application Level */
app_expr:
  | app_expr atomic_expr      { ExprNode (App ($1, $2)) }
  | FST atomic_expr           { ExprNode (First $2) }
  | SND atomic_expr           { ExprNode (Second $2) }
  | atomic_expr { $1 }        /* Fallthrough */
  ;

/* Atomic expressions (variables, constants, parens, tuples, distributions) */ 
atomic_expr:
  | n = number                { ExprNode (Const n) }
  | TRUE                      { ExprNode (BoolConst true) }
  | FALSE                     { ExprNode (BoolConst false) }
  | k = INT HASH n = INT
    { if k < 0 || k >= n then failwith (Printf.sprintf "Invalid FinConst: %d#%d" k n) else ExprNode (FinConst (k, n)) }
  | x = IDENT                 { ExprNode (Var x) }
  | UNIFORM LPAREN lo = number COMMA hi = number RPAREN
    { ExprNode (CDistr (Uniform (lo, hi))) }
  | GAUSSIAN LPAREN mean = number COMMA std = number RPAREN
    { ExprNode (CDistr (Gaussian (mean, std))) }
  | EXPONENTIAL LPAREN rate = number RPAREN
    { ExprNode (CDistr (Exponential rate)) }
  | BETA LPAREN alpha = number COMMA beta = number RPAREN
    { ExprNode (CDistr (Beta (alpha, beta))) }
  | DISCRETE LPAREN cases = distr_cases RPAREN
    { ExprNode (DistrCase cases) }
  | LPAREN e = expr RPAREN      { e }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { ExprNode (Pair (e1, e2)) }
  ;

/* Rule for parsing the (expr : number) pairs for DistrCase */ 
distr_cases:
  | /* empty */ { [] } 
  | cases = separated_nonempty_list(COMMA, distr_case) { cases }
  ;

distr_case:
  | p = number COLON e = expr { (e, p) }
  ;

number:
  | f = FLOAT { f }
  | i = INT   { float_of_int i }
  ;
