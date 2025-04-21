# ContDice - A Continuous Dice Language

ContDice is a language for probabilistic programming with continuous distributions.

# Usage

run.sh illustrates the workflow of the inference steps: first cdice is run on a continuous program, outputting a discretized program, then dice is run on the discretized program, outputting the final probability distribution.

```bash
$ ./run.sh <full_path_of_example_cdice_program>
```

## Running cdice alone
```bash
$ cd cdice
$ dune build
$ dune exec -- bin/main.exe [-print-all] <example_cdice_program>
```
Options:

-print-all: prints the intermediate steps in the cdice workflow up to the discretized program, i.e.:

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
```

