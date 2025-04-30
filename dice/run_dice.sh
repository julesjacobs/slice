#!/bin/bash
# Running dice with a bash script

if [ $# -lt 1 ]; then
  echo "Usage: $0 <string-or-file>"
  exit 1
fi

INPUT=$1
INPUT_ABS=$(realpath "$INPUT")

# if [ -f "$INPUT" ]; then
  # If arg is a file, read the file contents into a string
  # INPUT_ABS=$(realpath "$INPUT")
  # OUTPUT=$(cat "$INPUT_ABS")
# else
  # If arg is a string, use the string directly
  # OUTPUT="$INPUT"
# fi

# Now run dune exec dice
# dune exec dice "$INPUT_ABS" # build-step included
./_build/default/bin/dice.exe "../output.dice" # directly run the executable
