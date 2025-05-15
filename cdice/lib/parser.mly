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
%token LOGNORMAL
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
(* %token FOR TO DO *)

(* Precedence and associativity *)
%left SEMICOLON
%left PLUS MINUS
%left TIMES DIVIDE
%right COMMA


%start <Types.expr> prog

(* Define types for non-terminals *) 
%type <Types.expr> expr assign_expr cons_expr cmp_expr app_expr atomic_expr prefix_expr comma_expr
%type <unit> opt_bar
%type <float> number
%type <(Types.expr * float) list> distr_cases

%% 

prog: e = expr EOF { e };

/* Lowest precedence: LET, IF, FUN, FIX, MATCH (handled by structure) 
   then SEMICOLON, then assign_expr and higher operators */
expr:
    expr SEMICOLON expr 
    { ExprNode (Seq ($1, $3)) }
  | LET x = IDENT EQUAL e1 = expr IN e2 = expr
    { ExprNode (Let (x, e1, e2)) }
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
  | assign_expr { $1 } /* Fallthrough to higher precedence expression forms */
  ;

/* Assignment level */
assign_expr:
  | prefix_expr COLON_EQUAL assign_expr { ExprNode (Assign ($1, $3)) }
  | comma_expr { $1 }
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
  | cmp_expr LESS cons_expr     { ExprNode (Less ($1, $3)) }
  | cmp_expr LESSEQ cons_expr   { ExprNode (LessEq ($1, $3)) }
  | cons_expr LT_HASH INT cons_expr { ExprNode (FinLt ($1, $4, $3)) } 
  | cons_expr LEQ_HASH INT cons_expr { ExprNode (FinLeq ($1, $4, $3)) } 
  | cons_expr { $1 }            /* Fallthrough to cons_expr */
  ;

/* Cons level (right-associative) */
cons_expr:
  | prefix_expr CONS cons_expr   { ExprNode (Cons ($1, $3)) } (* Use prefix_expr here *) 
  | prefix_expr { $1 }           /* Fallthrough to prefix_expr */
  ;

/* Prefix operators level */
prefix_expr:                  (* New level for prefix ops like ! and ref *)
  | BANG prefix_expr          { ExprNode (Deref $2) }
  | REF prefix_expr           { ExprNode (Ref $2) }
  | app_expr { $1 }           /* Fallthrough to application */
  ;

/* Application Level */
app_expr:
  | app_expr atomic_expr      { ExprNode (App ($1, $2)) }
  | FST atomic_expr           { ExprNode (First $2) }
  | SND atomic_expr           { ExprNode (Second $2) }
  | atomic_expr { $1 }        /* Fallthrough */
  ;

/* Atomic expressions (variables, constants, parens, tuples, distributions, nil) */ 
atomic_expr:
  | n = number                { ExprNode (Const n) }
  | TRUE                      { ExprNode (BoolConst true) }
  | FALSE                     { ExprNode (BoolConst false) }
  | NIL                       { ExprNode Nil }
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
  | LOGNORMAL LPAREN mu = number COMMA sigma = number RPAREN
    { ExprNode (CDistr (LogNormal (mu, sigma))) }
  | DISCRETE LPAREN cases = distr_cases RPAREN
    { ExprNode (DistrCase cases) }
  | LPAREN e = expr RPAREN      { e }
  | GAMMA LPAREN shape = number COMMA scale = number RPAREN
    { ExprNode (CDistr (Gamma (shape, scale))) }
  | LAPLACE LPAREN scale = number RPAREN
    { ExprNode (CDistr (Laplace (scale))) }
  | CAUCHY LPAREN scale = number RPAREN
    { ExprNode (CDistr (Cauchy (scale))) }
  | WEIBULL LPAREN a = number COMMA b = number RPAREN
    { ExprNode (CDistr (Weibull (a, b))) }
  | TDIST LPAREN nu = number RPAREN
    { ExprNode (CDistr (TDist (nu))) }
  | CHI2 LPAREN nu = number RPAREN
    { ExprNode (CDistr (Chi2 (nu))) }
  | LOGISTIC LPAREN scale = number RPAREN
    { ExprNode (CDistr (Logistic (scale))) }
  | LPAREN RPAREN { ExprNode Unit }
  ;

/* Optional bar rule */
opt_bar:
  | /* empty */ { () }
  | BAR         { () }
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
  /* Arithmetic expressions */
  | e1 = number PLUS e2 = number { e1 +. e2 }
  | e1 = number MINUS e2 = number { e1 -. e2 }
  | e1 = number TIMES e2 = number { e1 *. e2 }
  | e1 = number DIVIDE e2 = number { e1 /. e2 }
  | LPAREN e = number RPAREN { e }

comma_expr:
  | comma_expr COMMA cmp_expr { ExprNode (Pair ($1, $3)) }
  | cmp_expr { $1 }
  ;

%% (* This %% should mark the end of rules and precede any OCaml code if present *)
