#!/bin/bash
# Running dice with a bash script

if [ $# -lt 1 ]; then
  echo "Usage: $0 [--time] <string-or-file>"
  exit 1
fi

# Default: do not use --time
USE_TIME=false

# Parse flags
while [[ $# -gt 0 ]]; do
  case "$1" in
    --time)
      USE_TIME=true
      shift
      ;;
    *)
      INPUT=$1
      shift
      ;;
  esac
done

if [ -z "$INPUT" ]; then
  echo "Error: missing input argument."
  echo "Usage: $0 [--time] <string-or-file>"
  exit 1
fi

INPUT_ABS=$(realpath "$INPUT")

# Run dice.exe with or without --time
if [ "$USE_TIME" = true ]; then
  ./_build/default/bin/dice.exe -time "../output.dice"
else
  ./_build/default/bin/dice.exe "../output.dice"
fi
