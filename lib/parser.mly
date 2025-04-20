%{
open Types
%}

%token <string> IDENT
%token <float> FLOAT
%token LET IN
%token IF THEN ELSE
%token UNIFORM
%token GAUSSIAN
%token EXPONENTIAL
%token BETA
%token DISCRETE
%token FST SND
%token FUN ARROW
%token LESS
%token LESSEQ
%token LPAREN RPAREN
%token COMMA
%token EQUAL
%token EOF

%start <Types.expr> prog

%%

prog:
  | e = expr EOF { e }
  ;

expr:
  | simple_expr { $1 }
  | LET x = IDENT EQUAL e1 = expr IN e2 = expr
    { ExprNode (Let (x, e1, e2)) }
  | IF cond = expr THEN e1 = expr ELSE e2 = expr
    { ExprNode (If (cond, e1, e2)) }
  | e = simple_expr LESS f = FLOAT
    { ExprNode (Less (e, f)) }
  | e = simple_expr LESSEQ n = FLOAT
    { ExprNode (LessEq (e, n)) }
  | FUN x = IDENT ARROW e = expr 
    { ExprNode (Fun (x, e)) }
  | e1 = simple_expr e2 = simple_expr
    { ExprNode (App (e1, e2)) }
  | FST e = simple_expr
    { ExprNode (First e) }
  | SND e = simple_expr
    { ExprNode (Second e) }
  ;

simple_expr:
  | x = IDENT 
    { ExprNode (Var x) }
  | UNIFORM LPAREN lo = FLOAT COMMA hi = FLOAT RPAREN
    { ExprNode (CDistr (Uniform (lo, hi))) }
  | GAUSSIAN LPAREN mean = FLOAT COMMA std = FLOAT RPAREN
    { ExprNode (CDistr (Gaussian (mean, std))) }
  | EXPONENTIAL LPAREN rate = FLOAT RPAREN
    { ExprNode (CDistr (Exponential rate)) }
  | BETA LPAREN alpha = FLOAT COMMA beta = FLOAT RPAREN
    { ExprNode (CDistr (Beta (alpha, beta))) }
  | DISCRETE LPAREN probs = separated_list(COMMA, FLOAT) RPAREN
    { ExprNode (Discrete probs) }
  | LPAREN e = expr RPAREN
    { e }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN
    { ExprNode (Pair (e1, e2)) }
  ;
