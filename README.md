# Slice - A Language for Type-Directed Discretization

Slice is a language for probabilistic programming with continuous distributions that uses type-directed discretization to enable exact inference.

# Usage

`run_slice.sh` outlines the inference workflow: first slice is run on a continuous program outputting a discretized program, then dice is run on the discretized program outputting the final probability distribution.

```bash
$ ./run_slice.sh <example-slice-program>
```

## Running slice alone
```bash
$ cd slice
$ dune build
$ dune exec -- bin/main.exe [--print-all] <example-slice-program>
```
Alternatively, can use the run script:

```bash
$ cd slice
$ ./run_slice.sh [--print-all] <example-slice-program>
```
Options:

--print-all: prints to stdout the intermediate steps in the slice workflow up to the discretized program, and performs a statistical verification test to determine equivalence between the discretized output and the sampled original using a Z-test, i.e.:

```bash
$ dune exec -- bin/main.exe examples/coin_flip.slice 

Processing file: examples/coin_flip.slice
Source:
let x = uniform(0, 1) in
x < 0.5

Parsed AST (Pretty):
let x = uniform(0, 1) in
x < 0.5

Typed AST (Pretty):
(let x = (uniform(0, 1) : float[<0.5; T]) in
((x : float[<0.5; T]) < (0.5 : float[<0.5; 0.5]) : bool) : bool)

Discretized Program (Pretty):
let x = discrete(0.5, 0.5) in
x <#2 1#2

Discretized Program (Plaintext):
let x = discrete(0.5, 0.5) in
x < int(1,1)

Running Discretized version 1000000 times...
Summary (Discretized): True: 500014, False: 499986
Running Original (Sampling) version 1000000 times...
Summary (Original (Sampling)): True: 499965, False: 500035

--- Statistical Comparison ---
Proportion True (Discretized): 0.5000
Proportion True (Original):   0.5000
Pooled Proportion:            0.5000
Z-score: 0.0693
Conclusion: No statistically significant difference found (alpha=0.01).
------------------------------------------------------------

*** No statistically significant difference detected for this file. ***
```

## Running dice alone
```bash
$ cd dice
$ dune build
$ dune exec dice <example-dice-program> # file or string
```
Alternatively, can use the run script:
```bash
$ cd dice
$ ./run_dice <example-dice-program> # file or string
```
