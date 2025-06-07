# Slice - A Language for Type-Directed Discretization

Slice is a language for probabilistic programming with continuous distributions that uses type-directed discretization to enable exact inference.

## Features

- Uniform distribution: `uniform(lo, hi)`
- Discrete distribution: `discrete(prob1: <expr1>, prob2: <expr2>, ..., probN: <exprN>)` where `prob1 + prob2 + ... + probN` should ideally be 1.0. The values returned are the results of evaluating the corresponding `<expr>`.
- Conditionals: `if <condition> then <expr> else <expr>`
- Variables: `let x = <expr> in <expr>`
- Functions: `fun x -> <expr>`
- Loops: `iterate`
- Recursive functions: `fix`
- Pairs: `(fst, snd)`
- Comparison operators: Simple comparison `<` (e.g., `x < 0.5`), and comparisons on finite types `<#type`, `<=#type`, `==#type` (e.g., `y <=#2 0#2`). Both operands must be of the same finite type `#type`.
- Comments: OCaml-style comments `(* ... *)`.

The implementation can attempt to discretize continuous distributions into discrete ones.

## Building

```bash
# Install dependencies (if not already installed via opam)

# opam install base stdio dune ppx_jane menhir

# Build the project
dune build
```

## Running

The main executable is `bin/main.exe`.

```bash
# Run on a single file
dune exec -- bin/main.exe examples/coin_flip.slice

# Run on a directory (processes all .slice files recursively)
dune exec -- bin/main.exe examples
```

Alternatively, use the provided helper script:

```bash
# Run all examples
./run.sh

# Run a specific example (e.g., coin.slice)
./run.sh coin.slice
```

## Examples

Check the `examples/` directory for more sample programs.

### Coin Flip Example

```ocaml
(* examples/coin.slice *)
let coin = discrete(0.5: 0#2, 0.5: 1#2) in  (* Returns 0 or 1 with equal probability *)
let u = uniform(0.0, 1.0) in
if coin <=#2 0#2 then
  u < 0.2  (* If coin is 0, check if u < 0.2 *)
else
  u < 0.8  (* If coin is 1, check if u < 0.8 *)
```

## Syntax

The language has a simple ML-inspired syntax:

```
e ::= x
    | let x = e1 in e2
    | uniform(f1, f2)
    | gaussian(f1, f2)
    | discrete(f1: e1, f2: e2, ...)
    | e < e'
    | e <= e'
    | e <#n e'
    | e <=#n e'
    | if e1 then e2 else e3
    | fun x -> e
    | (e)
    | x          (* variable *)
    | f           (* float literal* )
    | k#n      (* finite type literal, e.g., 0#2, 1#2 *)
```