# ContDice - A Continuous Dice Language

ContDice is a language for probabilistic programming with continuous distributions.

## Features

- Uniform distribution: `uniform(lo, hi)`
- Discrete distribution: `discrete(p1, p2, ..., pn)` where p1+p2+...+pn = 1.0
- Conditionals: `if <condition> then <expr> else <expr>`
- Variables: `let x = <expr> in <expr>`
- Comparison operators: `<` and `<=`

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

### Basic Coin Flip (Uniform)

```
let x = uniform(0, 1) in
if x < 0.5 then 0 else 1
```

### Basic Coin Flip (Discrete)

```
let coin = discrete(0.5, 0.5) in
if coin < 0.5 then 0 else 1
```

### Rolling a Die (Uniform)

```
let roll = uniform(1, 7) in
if roll < 2 then 1 else
if roll < 3 then 2 else
if roll < 4 then 3 else
if roll < 5 then 4 else
if roll < 6 then 5 else 6
```

### Rolling a Die (Discrete)

```
let roll = discrete(1.0/6.0, 1.0/6.0, 1.0/6.0, 1.0/6.0, 1.0/6.0, 1.0/6.0) in
roll
```

### Mixing Distribution Types

```
let coin = discrete(0.5, 0.5) in
let u = uniform(0.0, 1.0) in
if coin < 0.5 then
  u < 0.2
else
  u < 0.8
```

## Syntax

The language has a simple ML-inspired syntax:

```
<expr> ::= <identifier>
         | let <identifier> = <expr> in <expr>
         | uniform(<float>, <float>)
         | discrete(<float>, <float>, ...)
         | <expr> < <float>
         | <expr> <= <int>
         | if <expr> then <expr> else <expr>
         | (<expr>)
```