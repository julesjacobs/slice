#!/bin/bash
# Running dice with a bash script

if [ $# -lt 1 ]; then
  echo "Usage: $0 [--print-all] <cdice-file>"
  exit 1
fi

PRINT_ALL=false

# Check if the first argument is --print-all
if [ "$1" == "--print-all" ]; then
  PRINT_ALL=true
  shift  # Remove --print-all from the arguments
fi

if [ $# -lt 1 ]; then
  echo "Error: Missing <cdice-file> argument."
  exit 1
fi

INPUT=$1
INPUT_ABS=$(realpath "$INPUT")

if [ "$PRINT_ALL" = true ]; then
  dune exec -- bin/main.exe --print-all "$INPUT_ABS" | tee ../output.dice
else
  dune exec -- bin/main.exe "$INPUT_ABS" | tee ../output.dice
fi
