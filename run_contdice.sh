#!/bin/bash

# ================[ WORKFLOW FOR CONTDICE ]================
# First run cdice, which converts a continuous program to an eqyuivalent discretized one
# Then run dice, which performs the inference on the discretized program


# ================[ CDICE ]================
if [ $# -lt 1 ]; then
  echo "Usage: $0 <example-program>"
  exit 1
fi

EXAMPLE=$1
EXAMPLE_ABS=$(realpath "$EXAMPLE")

# Go into cdice directory and run the command
cd cdice || exit 1
OUTPUT=$(dune exec -- bin/main.exe "$EXAMPLE_ABS")

# ================[ DICE ]================
cd ..
echo "$OUTPUT" > output.dice

# Go into dice directory and run the command
cd dice || exit 1
dune exec dice "../output.dice"