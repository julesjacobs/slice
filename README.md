# ContDice - A Continuous Dice Language

ContDice is a language for probabilistic programming with continuous distributions.

## Features

- Uniform distribution: `uniform(lo, hi)`
- Conditionals: `if <condition> then <expr> else <expr>`
- Variables: `let x = <expr> in <expr>`
- Comparison operators: `<`

## Building

```bash
# Install dependencies
opam install menhir

# Build the project
dune build
```

## Running

```bash
# Run on a single file
dune exec -- contdice examples/coin_flip.cdice

# Run on a directory (processes all .cdice files recursively)
dune exec -- contdice examples
```

## Examples

Check the `examples/` directory for sample programs.

### Basic Coin Flip

```
let x = uniform(0, 1) in
if x < 0.5 then 0 else 1
```

### Rolling a Die

```
let roll = uniform(1, 7) in
if roll < 2 then 1 else
if roll < 3 then 2 else
if roll < 4 then 3 else
if roll < 5 then 4 else
if roll < 6 then 5 else 6
```

## Syntax

The language has a simple ML-inspired syntax:

```
<expr> ::= <identifier>
         | let <identifier> = <expr> in <expr>
         | uniform(<float>, <float>)
         | <expr> < <float>
         | if <expr> then <expr> else <expr>
         | (<expr>)
```