%{
open Ast

module StringSet = Set.Make (String)

(* [discrete(p0: e0, ..., pn: en)] is surface syntax.  Keep the AST and
   subsequent passes in the finite-index core by elaborating it to one index
   sample followed by finite comparisons. *)
let rec identifiers (ExprNode e) =
  let union = StringSet.union in
  match e with
  | Var x -> StringSet.singleton x
  | Const _ | BoolConst _ | FinConst _ | Unit | Nil | RuntimeError _ ->
      StringSet.empty
  | Let (x, e1, e2) ->
      StringSet.add x (union (identifiers e1) (identifiers e2))
  | Sample (Distr1 (_, e1)) -> identifiers e1
  | Sample (Distr2 (_, e1, e2)) -> union (identifiers e1) (identifiers e2)
  | DistrCase cases ->
      List.fold_left
        (fun acc (ei, _) -> union acc (identifiers ei))
        StringSet.empty cases
  | Cmp (_, e1, e2, _) | And (e1, e2) | Or (e1, e2) | Pair (e1, e2)
  | FuncApp (e1, e2) | Cons (e1, e2) | Assign (e1, e2) | Seq (e1, e2)
  | FinEq (e1, e2, _) | LoopApp (e1, e2, _) ->
      union (identifiers e1) (identifiers e2)
  | FinCmp (_, e1, e2, _, _) -> union (identifiers e1) (identifiers e2)
  | Not e1 | First e1 | Second e1 | Observe e1 | Ref e1 | Deref e1 ->
      identifiers e1
  | If (e1, e2, e3) ->
      union (identifiers e1) (union (identifiers e2) (identifiers e3))
  | Fun (x, body) -> StringSet.add x (identifiers body)
  | Fix (f, x, body) -> StringSet.add f (StringSet.add x (identifiers body))
  | MatchList (e, e_nil, h, t, e_cons) ->
      StringSet.add h
        (StringSet.add t
           (union (identifiers e)
              (union (identifiers e_nil) (identifiers e_cons))))

let fresh_choice_name cases =
  let avoid =
    List.fold_left
      (fun acc (ei, _) -> StringSet.union acc (identifiers ei))
      StringSet.empty cases
  in
  let rec choose i =
    let candidate = "__slice_discrete_choice_" ^ string_of_int i in
    if StringSet.mem candidate avoid then choose (i + 1) else candidate
  in
  choose 0

let elaborate_finite_support cases =
  match cases with
  | [] -> failwith "A finite-support discrete distribution cannot be empty"
  | _ ->
      let n = List.length cases in
      let choice_name = fresh_choice_name cases in
      let index_cases =
        List.mapi (fun i (_, p) -> (ExprNode (FinConst (i, n)), p)) cases
      in
      let rec select i = function
        | [] -> assert false
        | [ (ei, _) ] -> ei
        | (ei, _) :: rest ->
            let choice = ExprNode (Var choice_name) in
            let threshold = ExprNode (FinConst (i + 1, n)) in
            let test = ExprNode (FinCmp (Lt, choice, threshold, n, false)) in
            ExprNode (If (test, ei, select (i + 1) rest))
      in
      ExprNode
        (Let
           ( choice_name,
             distr_case ~policy:Rounded_input index_cases,
             select 0 cases ))
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
%token GREATER
%token GREATEREQ
%token HASH
%token LT_HASH
%token LEQ_HASH
%token GT_HASH
%token GEQ_HASH
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
%token POISSON
%token BINOMIAL
%token ITERATE

%start <Ast.expr> prog

(* Define types for non-terminals *) 
%type <Ast.expr> expr assign_expr cons_expr cmp_expr app_expr atomic_expr prefix_expr
%type <Ast.expr list> expr_comma_list (* New type for list of expressions *)
%type <unit> opt_bar
%type <float> number
%type <(Ast.expr * float) list> distr_cases

(* Operator precedence and associativity from commit 23e2e6c *)
%left ARROW             (* Function type arrow *)
%left SEMICOLON
%left PLUS MINUS
%left TIMES DIVIDE
%left OR                (* OR has lower precedence than AND *)
%left AND               (* AND has lower precedence than NOT/comparison *)
%right NOT              (* Unary NOT has high precedence *)
%right COMMA
%nonassoc LESS LESSEQ GREATER GREATEREQ LT_HASH LEQ_HASH GT_HASH GEQ_HASH EQ_HASH (* Comparison operators *)

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
  | assign_expr { $1 } /* Fallthrough to assign_expr */
  ;

/* Assignment level */
assign_expr:
  | prefix_expr COLON_EQUAL assign_expr { ExprNode (Assign ($1, $3)) }
  | or_expr { $1 } /* Fallthrough to or_expr, as per diff */
  ;

/* OR Level */
or_expr:
  | or_expr OR and_expr  { ExprNode (Or ($1, $3)) }
  | and_expr { $1 }     /* Fallthrough to and_expr */
  ;

/* AND Level */
and_expr:
  | and_expr AND not_expr { ExprNode (And ($1, $3)) }
  | not_expr { $1 }      /* Fallthrough to not_expr */
  ;

/* NOT Level */
not_expr:
  | NOT not_expr          { ExprNode (Not $2) }
  | cmp_expr { $1 }       /* Fallthrough to cmp_expr */
  ;

/* Comparison level */
cmp_expr:
  | cmp_expr LESS cons_expr     { ExprNode (Cmp (Lt, $1, $3, false)) }     (* Original < *)
  | cmp_expr LESSEQ cons_expr   { ExprNode (Cmp (Le, $1, $3, false)) }     (* Original <= *)
  | cmp_expr GREATER cons_expr  { ExprNode (Cmp (Lt, $3, $1, true)) }      (* Flipped > to < *)
  | cmp_expr GREATEREQ cons_expr { ExprNode (Cmp (Le, $3, $1, true)) }     (* Flipped >= to <= *)
  | cons_expr LT_HASH INT cons_expr { ExprNode (FinCmp (Lt, $1, $4, $3, false)) }   (* Original <#n *)
  | cons_expr LEQ_HASH INT cons_expr { ExprNode (FinCmp (Le, $1, $4, $3, false)) }  (* Original <=#n *)
  | cons_expr GT_HASH INT cons_expr { ExprNode (FinCmp (Lt, $4, $1, $3, true)) }    (* Flipped >#n to <#n *)
  | cons_expr GEQ_HASH INT cons_expr { ExprNode (FinCmp (Le, $4, $1, $3, true)) }   (* Flipped >=#n to <=#n *)
  | cons_expr EQ_HASH INT cons_expr { ExprNode (FinEq ($1, $4, $3)) }
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
  | app_expr { $1 }           /* Fallthrough to app_expr */
  ;

/* Application Level */
app_expr:
  | app_expr atomic_expr                                                        { ExprNode (FuncApp ($1, $2)) }
  | ITERATE LPAREN e1 = app_expr COMMA e2 = atomic_expr COMMA n = INT RPAREN    { ExprNode (LoopApp (e1, e2, n)) }
  | FST atomic_expr                                                             { ExprNode (First $2) }
  | SND atomic_expr                                                             { ExprNode (Second $2) }
  | atomic_expr                                                                 { $1 }        /* Fallthrough to atomic_expr */
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
  | DISCRETE LPAREN probs = separated_nonempty_list(COMMA, number) RPAREN
    { 
      (* New syntax: discrete(p1, p2, ...) returns Fin values *)
      let n = List.length probs in
      let cases = List.mapi (fun i p -> (ExprNode (FinConst (i, n)), p)) probs in
      distr_case ~policy:Rounded_input cases
    }
  | DISCRETE LPAREN cases = distr_cases RPAREN
    { 
      (* Surface finite-support syntax, elaborated to the finite-index core. *)
      elaborate_finite_support cases
    }
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
  | POISSON LPAREN mu = app_expr RPAREN
    { ExprNode (Sample (Distr1 (DPoisson, mu))) }
  | BINOMIAL LPAREN arg1 = app_expr COMMA arg2 = app_expr RPAREN
    { ExprNode (Sample (Distr2 (DBinomial, arg1, arg2))) }
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

/* Optional bar rule */
opt_bar:
  | /* empty */ { () }
  | BAR         { () }
  ;

/* Surface finite-support cases [probability : expression]. */
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
  ;

expr_comma_list: (* Definition for comma-separated list of expressions *)
    e = expr                         { [e] }
  | e = expr COMMA rest = expr_comma_list { e :: rest }
  ;

%% (* This %% should mark the end of rules and precede any OCaml code if present *)
