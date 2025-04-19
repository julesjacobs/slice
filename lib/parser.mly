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

%nonassoc LESS LESSEQ
%nonassoc ELSE
%nonassoc THEN
%nonassoc IF
%right IN
%left EQUAL
%right ARROW
%left APP

%start <Types.expr> prog

%%

prog:
  | e = expr EOF { e }
  ;

expr:
  | simple_expr { $1 }
  | LET x = IDENT EQUAL e1 = expr IN e2 = expr
    { Let (x, e1, e2) }
  | IF cond = expr THEN e1 = expr ELSE e2 = expr
    { If (cond, e1, e2) }
  | e = simple_expr LESS f = FLOAT
    { Less (e, f) }
  | e = simple_expr LESSEQ n = FLOAT
    { LessEq (e, int_of_float n) }
  | FUN x = IDENT ARROW e = expr 
    { Fun (x, e) }
  | e1 = simple_expr e2 = simple_expr %prec APP
    { App (e1, e2) }
  | FST e = simple_expr
    { First e }
  | SND e = simple_expr
    { Second e }
  ;

simple_expr:
  | x = IDENT 
    { Var x }
  | UNIFORM LPAREN lo = FLOAT COMMA hi = FLOAT RPAREN
    { CDistr (Uniform (lo, hi)) }
  | GAUSSIAN LPAREN mean = FLOAT COMMA std = FLOAT RPAREN
    { CDistr (Gaussian (mean, std)) }
  | EXPONENTIAL LPAREN rate = FLOAT RPAREN
    { CDistr (Exponential rate) }
  | BETA LPAREN alpha = FLOAT COMMA beta = FLOAT RPAREN
    { CDistr (Beta (alpha, beta)) }
  | DISCRETE LPAREN probs = separated_list(COMMA, FLOAT) RPAREN
    { Discrete probs }
  | LPAREN e = expr RPAREN
    { e }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN
    { Pair (e1, e2) }
  ;
