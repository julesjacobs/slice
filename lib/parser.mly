%{
open Types
%}

%token <string> IDENT
%token <float> FLOAT
%token LET IN
%token IF THEN ELSE
%token UNIFORM
%token LESS
%token LPAREN RPAREN
%token COMMA
%token EQUAL
%token EOF

%nonassoc LESS
%nonassoc ELSE
%nonassoc THEN
%nonassoc IF
%right IN
%left EQUAL

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
  ;

simple_expr:
  | x = IDENT 
    { Var x }
  | UNIFORM LPAREN lo = FLOAT COMMA hi = FLOAT RPAREN
    { Uniform (lo, hi) }
  | LPAREN e = expr RPAREN
    { e }
  ;