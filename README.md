# ContDice - A Continuous Dice Language

ContDice is a language for probabilistic programming with continuous distributions.

# Usage

`run_contdice.sh` outlines the inference workflow: first cdice is run on a continuous program outputting a discretized program, then dice is run on the discretized program outputting the final probability distribution.

```bash
$ ./run_contdice.sh <example-cdice-program>
```

## Running cdice alone
```bash
$ cd cdice
$ dune build
$ dune exec -- bin/main.exe [--print-all] <example-cdice-program>
```
Alternatively, can use the run script:

```bash
$ cd cdice
$ ./run_cdice.sh [--print-all] <example-cdice-program>
```
Options:

--print-all: prints to stdout the intermediate steps in the contdice workflow up to the discretized program, and performs a statistical verification test to determine equivalence between the discretized output and the sampled original using a Z-test, i.e.:

```bash
$ dune exec -- bin/main.exe examples/coin_flip.cdice 

Processing file: examples/coin_flip.cdice
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
let x = discrete(0.5: 0#2, 0.5: 1#2) in
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
